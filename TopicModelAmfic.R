cleanText<-function(original.text, dict, sw){
  original.text<-unlist(strsplit(original.text, ""))
  original.text<-tolower(original.text)
  original.text<-original.text[which(original.text %in% c(letters, " "))]
  original.text<-paste(original.text, collapse="")
  original.text<-unlist(strsplit(original.text, " "))
  original.text<-original.text[which(original.text %in% dict)]
  sws<-which(original.text %in% sw)
  if(length(sws)>0){
    original.text<-original.text[-sws]
  }
  return(original.text)
}

splitText<-function(og.filename, output.folder, dict, sw, index.counter, split.size=1000){
  if((index.counter %% 100)==0){
    print(index.counter)
  }
  og.text<-scan(og.filename, what='character', sep="\n", quiet=T)
  clean.text<-cleanText(og.text, dict, sw)
  start.index<-seq(1,length(clean.text), by=1000)
  end.index<-start.index[2:length(start.index)]+1
  end.index<-c(end.index, length(clean.text))
  bad.ends<-which(end.index>length(clean.text))
  if(length(bad.ends)>0){
    end.index<-end.index[-bad.ends]
    start.index<-start.index[1:length(end.index)]
  }
  size.check<-length(end.index)
  if(size.check>0){
    seg.size.check<-end.index-start.index
    seg.size.flag<-which(seg.size.check<1)
    if(length(seg.size.flag>0)){
      start.index<-start.index[-seg.size.flag]
      end.index<-end.index[-seg.size.flag]
    }
    if (length(start.index)==1){
      end.index<-length(clean.text)
    }
    text.segments<-mapply(function(x,y) clean.text[x:y], start.index, end.index, SIMPLIFY=F)
    text.segments<-lapply(text.segments, function(x) paste(x, collapse=" "))
    filename.base<-unlist(strsplit(og.filename, "/"))
    filename.base<-filename.base[length(filename.base)]
    filename.base<-unlist(strsplit(filename.base, ".txt"))
    split.index<-as.character(seq(1, length(text.segments), by=1))
    filename.base<-paste(filename.base, split.index, sep="_")
    filename.base<-paste(filename.base, ".txt", sep="")
    filename.base<-paste(output.folder, filename.base, sep="/")
    all.write<-mapply(function(x,y) write(x,y), text.segments, filename.base)
  }
}

splitAmFic<-function(input.folder, output.folder, dict){
  library(tm)
  sw<-stopwords("en")
  all.files<-list.files(input.dir, full.names=T)
  index.count<-seq(1,length(all.files), by=1)
  all.file.write<-mapply(function(x,y) splitText(x, output.folder, dict, sw, y),  all.files, index.count)
}

#code  to build corpus out of the split model above
getTopicTexts<-function(model.pp, metadata.table, topic.number, threshold){
  topic.number<-topic.number+1
  pp.rownames<-rownames(model.pp)
  pp.rownames<-lapply(pp.rownames, function(x) unlist(strsplit(x, "_")))
  pp.filecore<-unlist(lapply(pp.rownames, function(x) x[1]))
  topic.subtable<-data.frame(pp.filecore, model.pp[,topic.number], stringsAsFactors = F)
  topic.subtable<-topic.subtable[order(topic.subtable[,2], decreasing=T),]
  topic.subtable<-topic.subtable[which(topic.subtable[,2]>threshold),]
  corpus.fn<-topic.subtable[,1]
  metadata.index<-unlist(lapply(corpus.fn, function(x) grep(x, metadata.table$Filename)))
  #metadata.index<-metadata.index[-which(duplicated(metadata.index))]
  sub.corpus.meta<-metadata.table[metadata.index,]
  sub.corpus.meta<-sub.corpus.meta[,c(3,4,6)]
  rownames(sub.corpus.meta)<-NULL
  print(sort(table(sub.corpus.meta[,2]), decreasing=T))
  return(sub.corpus.meta)
}

#code to build a corpus from a topicPP table and a metadata table
#requires a vector of topics, a threshold and a boolean indicating whether the corpus is made of excerpts  or full books
#also indicates a max corpus size
#NOTE: for multi-topic corpora, code puts all topics together and takes the top results across topics (rather than the top x topics per topic)
#this may need an option to change
#directory structure is created: in the output.folder, the program places a metadata file with the format corpusname_metadata.csv
#then it creates a folder with corpusname with text folders inside. Filename in the metadata table is createdfolder/filename.txt
buildCorpus<-function(topic.pp, corpus.metadata, topic.vector, pp.limit, source.folder, output.folder, corpus.name, excerpt=F, max.corpus.size=100){
  #mallet tables are indexed from 0, so add 1 to the topic vector to get columns
  topic.vector<-topic.vector+1
  topic.dfs<-lapply(topic.vector, function(x) data.frame(rownames(topic.pp), topic.pp[,x], stringsAsFactors = F))
  topic.dfs<-do.call("rbind", topic.dfs)
  topic.dfs<-topic.dfs[order(topic.dfs[,2], decreasing=T),]
  topic.dfs<-topic.dfs[which(topic.dfs[,2]>=pp.limit),]
  filenames.split<-lapply(topic.dfs[,1], function(x) unlist(strsplit(x, "_")))
  base.filenames<-unlist(lapply(filenames.split, function(x) x[1]))
  output.folder.name<-paste(output.folder, corpus.name, sep="/")
  dir.create(output.folder.name)
  if(excerpt){
    if(nrow(topic.dfs)>max.corpus.size){
      topic.dfs<-topic.dfs[1:max.corpus.size,]
      base.filenames<-base.filenames[1:max.corpus.size]
      filenames.split<-filenames.split[1:max.corpus.size]
    }
    segments<-unlist(lapply(filenames.split, function(x) x[2]))
    segments<-unlist(strsplit(segments, ".txt"))
    metadata.titles<-unlist(lapply(base.filenames, function(x) corpus.metadata$Title[grep(x, corpus.metadata$Filename)]))
    metadata.dates<-unlist(lapply(base.filenames, function(x) corpus.metadata$Date[grep(x, corpus.metadata$Filename)]))
    metadata.authors<-unlist(lapply(base.filenames, function(x) corpus.metadata$Author[grep(x, corpus.metadata$Filename)]))
    new.corpus.metadata<-data.frame(topic.dfs[,1], segments, metadata.titles, metadata.authors, metadata.dates, stringsAsFactors=F)
    colnames(new.corpus.metadata)<-c("Filename", "Segment", "Title", "Author", "Date")
    source.files<-paste(source.folder, new.corpus.metadata$Filename, sep="/")
    full.path.fn<-paste(output.folder.name, new.corpus.metadata$Filename, sep="/")
    new.corpus.metadata$Filename<-full.path.fn
    metadata.filename<-paste(output.folder, "/", corpus.name, "_metadata.csv", sep="")
    write.csv(new.corpus.metadata, file=metadata.filename, row.names=F)
    #file.copy(source.files, output.folder.name)
  } else {
    base.filenames<-base.filenames[-which(duplicated(base.filenames))]
    if(length(base.filenames)>max.corpus.size){
      base.filenames<-base.filenames[1:max.corpus.size]
    }
    metadata.titles<-unlist(lapply(base.filenames, function(x) corpus.metadata$Title[grep(x, corpus.metadata$Filename)]))
    metadata.dates<-unlist(lapply(base.filenames, function(x) corpus.metadata$Date[grep(x, corpus.metadata$Filename)]))
    metadata.authors<-unlist(lapply(base.filenames, function(x) corpus.metadata$Author[grep(x, corpus.metadata$Filename)]))
    base.filenames<-paste(base.filenames, ".txt", sep="")
    source.files<-paste(source.folder, base.filenames, sep="/")
    output.filenames<-paste(output.folder.name, base.filenames, sep="/")
    new.corpus.metadata<-data.frame(output.filenames, metadata.titles, metadata.authors, metadata.dates, stringsAsFactors=F)
    colnames(new.corpus.metadata)<-c("Filename", "Title", "Author", "Date")
    write.csv(new.corpus.metadata, paste(output.folder, "/", corpus.name, "_metadata.csv", sep=""), row.names=F)
    #file.copy(source.files, output.folder.name)
  }
}

