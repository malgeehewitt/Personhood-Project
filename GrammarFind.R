parseTexts<-function(input.filename, source.folder="/users/malgeehe/dropbox/CHICAGO_CORPUS/CHICAGO_NOVEL_CORPUS", output.folder="/Users/malgeehe/Dropbox/Transfer/Projects/Personhood/ChicagoModel"){
  library(udpipe)
  full.input.filename<-paste(source.folder, input.filename, sep="/")
  output.filename<-unlist(strsplit(input.filename, ".txt"))
  output.filename<-paste(output.filename, "parsed.csv", sep="_")
  output.filename<-paste(output.folder, output.filename, sep="/")
  input.file<-scan(full.input.filename, what='character', sep="\n", quiet=T)
  input.file<-paste(input.file, collapse=" ")
  full.parse<-udpipe(input.file, "english")
  write.csv(full.parse, output.filename, row.names=F)
}


###########


#function takes a list of keywords and retrieves syntax and attached terms from a parsed table (above)
#syntax of feature is dependency_word
#master.list is a list of vectors 
retrieveFeatures<-function(parsed.table.filename, wordlist){
  #ptm<-proc.time()
  parsed.table<-read.csv(parsed.table.filename, header=T, stringsAsFactors=F)
  #parsed.table<-parsed.table[-which(parsed.table$upos =='PUNCT'),]
  sent.token<-paste(parsed.table$sentence_id, parsed.table$token_id, sep="_")
  #governing.sent.token<-paste(parsed.table$sentence_id, parsed.table$head_token_id)
  table.terms<-tolower(parsed.table$token)
  hit.terms<-which(table.terms %in% wordlist)
  if(length(hit.terms)>0){
    hit.deps<-parsed.table$dep_rel[hit.terms]
    root.hits<-which(hit.deps=="root")
    if(length(root.hits)>0){
      hit.terms<-hit.terms[-root.hits]
      hit.deps<-hit.deps[-root.hits]
    }
    og.terms<-table.terms[hit.terms]
    hit.links<-paste(parsed.table$sentence_id[hit.terms], parsed.table$head_token_id[hit.terms], sep="_")
    link.tokens<-lapply(hit.links, function(x) parsed.table$lemma[which(sent.token==x)])
    link.tokens<-unlist(link.tokens)
    link.tokens<-tolower(link.tokens)
    final.vector<-paste(hit.deps, link.tokens, sep="_")
    final.vector<-paste(og.terms, final.vector, sep="-")
    #final.vector<-data.frame(og.terms, hit.deps, link.tokens, hit.links, stringsAsFactors=F)
    #print(proc.time()-ptm)
    return(final.vector)
  }
}

collectFeatures<-function(source.folder, word.list, output.folder){
  ptm<-proc.time()
  all.tables<-list.files(source.folder, full.names=T)
  library(parallel)
  ncore<-detectCores()-1
  clust.proc<-makeCluster(ncore, type="FORK")
  all.results<-parLapply(clust.proc, all.tables, function(x) retrieveFeatures(x, word.list))
  stopCluster(clust.proc)
  #all.results<-lapply(all.tables, function(x) retrieveFeatures(x, word.list))
  all.results<-unlist(all.results)
  split.results<-lapply(all.results, function(x) unlist(strsplit(x, "-")))
  hit.words<-unlist(lapply(split.results, function(x) x[1]))
  hit.result<-unlist(lapply(split.results, function(x) x[2]))
  remove(split.results)
  remove(all.results)
  feature.table<-table(hit.result)
  feature.table<-sort(feature.table, decreasing=T)
  master.features<-names(feature.table[1:50000])
  base.features<-rep(0, length(master.features))
  names(base.features)<-master.features
  feature.hits<-which(hit.result %in% master.features)
  hit.words<-hit.words[feature.hits]
  hit.result<-hit.result[feature.hits]
  word.list<-word.list[which(word.list %in% hit.words)]
  wordlist.features<-lapply(word.list, function(x) table(hit.result[which(hit.words==x)]))
  wordlist.features<-lapply(wordlist.features, function(x) c(x, base.features))
  wordlist.features<-lapply(wordlist.features, function(x) tapply(x, names(x), "sum"))
  feature.table<-do.call('rbind', wordlist.features)
  rownames(feature.table)<-word.list
  write.csv(feature.table, file=paste(output.folder, "AdditionalWordsFeatureTable_AmFic.csv", sep="/"))
  print(proc.time()-ptm)
  return(feature.table)
}



