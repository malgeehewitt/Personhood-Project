#build out a vocabulary list from a word vector
buildWord<-function(vector.model, word, limit){
  if(word %in% rownames(vector.model)){
    word.buildout<-getTopTerms(vector.model, word, limit)
    word.buildout<-names(word.buildout)
    return(word.buildout)
  }
}

wordlistBuildout<-function(vector.model, word.list, limit){
  word.list<-tolower(word.list)
  culled.wordlist<-word.list[which(word.list %in% rownames(vector.model))]
  all.words<-lapply(word.list, function(x) buildWord(vector.model, x, limit))
  all.words<-table(unlist(all.words))
  all.words<-sort(all.words, decreasing=T)
  return(all.words)
}

multiObjectBuildout<-function(vector.model, wordlist.table, limit=10, buildout.cull=4){
  object.categories<-unique(wordlist.table$Category)
  all.wordlists<-lapply(object.categories, function(x) wordlist.table$Harvard_term[which(wordlist.table$Category==x)])
  #all.wordlists<-lapply(all.wordlists, function(x) sample(x, 5))
  all.buildouts<-lapply(all.wordlists, function(x) wordlistBuildout(vector.model, x, limit))
  culled.buildouts<-lapply(all.buildouts, function(x) x[which(x>buildout.cull)])
  culled.buildouts<-lapply(culled.buildouts, function(x) names(x))
  culled.buildouts<-mapply(function(x,y) x[which(!x %in% y)], culled.buildouts, all.wordlists, SIMPLIFY=F)
  wordlist.labels<-mapply(function(x,y) rep(x,length(y)), object.categories, culled.buildouts, SIMPLIFY = F)
  final.table<-data.frame(unlist(culled.buildouts), unlist(wordlist.labels), stringsAsFactors = F)
  colnames(final.table)<-c("BuildTerms", "Category")
  rownames(final.table)<-NULL
  write.csv(final.table, file="Amfic_BuildoutTable_limit10_cull4.csv")
  return(final.table)
}

#functions for extracting information from texts split on the word list
# library(openNLP)
# library(openNLPdata)
# library(NLP)
# library(tm)
# sent_anno<-Maxent_Sent_Token_Annotator()


getCollocates<-function(split.text, wordlist, horizon){
  target.index<-which(split.text %in% wordlist)
  if(length(target.index)<1){
    return(NA)
  } else {
    target.start<-target.index-horizon
    bad.values<-which(target.start<1)
    if(length(bad.values)>0){
      target.start[bad.values]<-1
    }
    target.end<-target.index+horizon
    bad.values<-which(target.end>length(split.text))
    if(length(bad.values>0)){
      target.end[bad.values]<-length(split.text)
    }
    all.excerpts<-mapply(function(x,y) split.text[x:y], target.start, target.end, SIMPLIFY = F)
    all.excerpts<-unlist(all.excerpts)
    all.excerpts<-paste(all.excerpts, collapse=" ")
    return(all.excerpts)
  }
}

objectExtract<-function(text.filename, wordlists){
  print(text.filename)
  ptm<-proc.time()
  human.words<-wordlists$BuildTerms[which(wordlists$Category=="Human")]
  animal.words<-wordlists$BuildTerms[which(wordlists$Category=="Animal")]
  object.words<-wordlists$BuildTerms[which(wordlists$Category=="Object")]
  full.text<-scan(text.filename, what='character', sep='\n', quiet=T, encoding="UTF-8")
  full.text<-paste(full.text, collapse=" ")
  full.text<-as.String(full.text)
  sentence.anno<-annotate(full.text, sent_anno)
  all.sentences<-full.text[sentence.anno]
  human.index<-unlist(lapply(human.words, function(x) grep(x, all.sentences)))
  animal.index<-unlist(lapply(animal.words, function(x) grep(x, all.sentences)))
  object.index<-unlist(lapply(object.words, function(x) grep(x, all.sentences)))
  dupe.index<-which(duplicated(human.index))
  if(length(dupe.index)>0){
    human.index<-human.index[-dupe.index]
  }
  dupe.index<-which(duplicated(animal.index))
  if(length(dupe.index)>0){
    animal.index<-animal.index[-dupe.index]
  }
  dupe.index<-which(duplicated(object.index))
  if(length(dupe.index)>0){
    object.index<-object.index[-dupe.index]
  }
  if(length(human.index)>0){
    human.sentences<-all.sentences[human.index]
    human.sentences<-paste(human.sentences, collapse=" ")
    test<-unlist(strsplit(human.sentences, " "))
  } else {
    human.sentences<-NA
  }
  if(length(animal.index)>0){
    animal.sentences<-all.sentences[animal.index]
    animal.sentences<-paste(animal.sentences, collapse=" ")
  } else {
    animal.sentences<-NA
  }
  if(length(object.index)>0){
    object.sentences<-all.sentences[object.index]
    object.sentences<-paste(object.sentences, collapse=" ")
  } else {
    object.sentences<-NA
  }
  text.table<-data.frame(c("Human", "Animal", "Object"), c(human.sentences, animal.sentences, object.sentences), stringsAsFactors=F)
  colnames(text.table)<-c("Category", "Text")
  print(proc.time()-ptm)
  return(text.table)
}

objectExtractExcerpt<-function(text.filename, wordlists, horizon=5){
  human.words<-wordlists$BuildTerms[which(wordlists$Category=="Human")]
  animal.words<-wordlists$BuildTerms[which(wordlists$Category=="Animal")]
  object.words<-wordlists$BuildTerms[which(wordlists$Category=="Object")]
  full.text<-scan(text.filename, what='character', sep='\n', quiet=T, encoding="UTF-8")
  full.text<-paste(full.text, collapse=" ")
  full.text<-unlist(strsplit(full.text, " "))
  human.excerpt<-getCollocates(full.text, human.words, horizon)
  animal.excerpt<-getCollocates(full.text, animal.words, horizon)
  object.excerpt<-getCollocates(full.text, object.words, horizon)
  text.table<-data.frame(c("Human", "Animal", "Object"), c(human.excerpt, animal.excerpt, object.excerpt), stringsAsFactors=F)
  colnames(text.table)<-c("Category", "Text")
  return(text.table)
}


corpusBuild<-function(source.folder, wordlists, dictionary, corpus.name, excerpt=F){
  all.files<-list.files(source.folder, full.names=T)
  #all.files<-sample(all.files, 10)
  if(!excerpt){
    all.texts<-lapply(all.files, function(x) objectExtract(x, wordlists))
    all.tables<-do.call("rbind", all.texts)
    missing.values<-which(is.na(all.tables$Text))
    if(length(missing.values)>0){
      all.tables<-all.tables[-missing.values,]
    }
  } else {
    all.texts<-lapply(all.files, function(x) objectExtractExcerpt(x, wordlists))
    all.tables<-do.call("rbind", all.texts)
    missing.values<-which(is.na(all.tables$Text))
    if(length(missing.values)>0){
      all.tables<-all.tables[-missing.values,]
    }
  }
  groups<-all.tables$Category
  all.texts<-all.tables$Text
  dict.ed<-dictionary[-which(dictionary %in% wordlists$BuildTerms)]
  sw<-stopwords("en")
  dict.ed<-dict.ed[-which(dict.ed %in% sw)]
  all.texts<-unlist(lapply(all.texts, function(x) cleanText(x, dictionary)))
  all.texts<-Corpus(VectorSource(all.texts))
  all.texts<-DocumentTermMatrix(all.texts)
  all.texts<-as.matrix(all.texts)
  corpus.mdws<-getGroupMDWs(all.texts, groups)
  write.csv(corpus.mdws, corpus.name, row.names=F)
  return(corpus.mdws)
}

cleanText<-function(sentence, dictionary){
  sentence<-unlist(strsplit(sentence, ""))
  sentence<-tolower(sentence)
  sentence<-sentence[which(sentence %in% c(letters, LETTERS, " "))]
  sentence<-paste(sentence, collapse="")
  sentence<-unlist(strsplit(sentence, " "))
  sentence<-sentence[which(sentence %in% dictionary)]
  sentence<-paste(sentence, collapse=" ")
  return(sentence)
}

kwicks<-function(og.text){
  full.text<-scan(text.filename, what='character', sep='\n', quiet=T, encoding="UTF-8")
  full.text<-paste(full.text, collapse=" ")
}