verbs<-c("VB", "VBG", "VBP", "VBZ", "VBD", "VBN", "MD")
present.verbs<-c("VB", "VBG", "VBP", "VBZ")
past.verbs<-c("VBD", "VBN")
modals<-"MD"
nouns<-c("NN", "NNS", "NNP", "NNPS")
adjectives<-c("JJ", "JJR", "JJS")
adverbs<-c("RB", "RBR", "RBS")
all.forms<-c(verbs, nouns, adjectives, adverbs)

#collects terms from a sentence node in a CoreNLP parsed document 
#allow.POS ensures that only certain POS are collected
collectTerms<-function(sentence.node, human.list, object.list, allow.POS){
  all.words<-unlist(xpathSApply(sentence.node, "tokens/token/word", xmlValue))
  all.pos<-unlist(xpathSApply(sentence.node, "tokens/token/POS", xmlValue))
  human.hit<-length(which(all.words %in% human.list))
  object.hit<-length(which(all.words %in% object.list))
  if(human.hit>0){
    if(object.hit==0){
      all.words<-all.words[which(all.pos %in% allow.POS)]
      all.words<-all.words[-which(all.words %in% human.list)]
      all.words<-paste(all.words,collapse=" ")
      return.list<-list("human", all.words)
      return(return.list)
    }
  } else  {
    if(object.hit>0){
      all.words<-all.words[which(all.pos %in% allow.POS)]
      all.words<-all.words[-which(all.words %in% object.list)]
      all.words<-paste(all.words,collapse=" ")
      return.list<-list("object", all.words)
      return(return.list)
    }
  }
}






textParse<-function(parsed.filename, human.list, object.list, allow.POS){
  nlp.file<-xmlTreeParse(parsed.filename, useInternalNodes = T)
  nlp.sentences<-getNodeSet(nlp.file, "//*/sentence")
  text.terms<-lapply(nlp.sentences, function(x) collectTerms(x, human.list, object.list, allow.POS))
  all.groups<-unlist(lapply(text.terms, function(x) x[[1]]))
  all.terms<-unlist(lapply(text.terms, function(x) x[[2]]))
  #all.terms<-unlist(lapply(all.terms, function(x) paste(x, collapse=" ")))
  text.list<-list(all.groups, all.terms)
  return(text.list)
}

corpusParse<-function(source.folder, human.list, object.list, allow.POS){
  all.files<-list.files(source.folder,full.names=T)
  all.parsed.sentences<-lapply(all.files, function(x) textParse(x, human.list, object.list, allow.POS))
  all.groups<-unlist(lapply(all.parsed.sentences, function(x) x[1]))
  all.terms<-unlist(lapply(all.parsed.sentences, function(x) x[2]))
  final.list<-list(all.groups, all.terms)
  return(final.list)
}

#***************************
#Alternate to the code above, but instead of using unique tokens, syntaical function is used
syntaxParse<-function(dep.node, human.list, object.list){
  syntax.parts<-xpathSApply(dep.node, "dep", xmlAttrs)
  dependents<-xpathSApply(dep.node, "dep/dependent", xmlValue)
  human.hit<-which(dependents %in% human.list)
  #print(human.hit)
  if(length(human.hit)>0){
    human.syntax<-syntax.parts[human.hit]
    human.names<-dependents[human.hit]
    category<-rep("human", length(human.hit))
    final.table<-data.frame(category, human.names, human.syntax, stringsAsFactors=F)
    colnames(final.table)<-c("Category", "Name", "Syntax")
  }
  object.hit<-which(dependents %in% object.list)
  #print(object.hit)
  if(length(object.hit)>0){
    object.syntax<-syntax.parts[object.hit]
    object.names<-dependents[object.hit]
    category<-rep("object", length(object.hit))
    object.table<-data.frame(category, object.names, object.syntax, stringsAsFactors = F)
    colnames(object.table)<-c("Category", "Name", "Syntax")
    if(length(human.hit)>1){
      final.table<-rbind(final.table, object.table)
    } else {
      final.table<-object.table
    }
  }
  if((length(human.hit)+length(object.hit))>0){
    return(final.table)
  }
}

depParse<-function(text.filename, human.list, object.list){
  xml.file<-xmlTreeParse(text.filename, useInternalNodes = T)
  dep.nodes<-getNodeSet(test.xml, "//*/dependencies[@type='basic-dependencies']")
  syntax.table<-lapply(dep.nodes, function(x) syntaxParse(x, human.list, object.list))
  syntax.table<-do.call("rbind", syntax.table)
  return(syntax.table)
}

corpusDepParse<-function(source.folder, human.list, object.list){
  all.files<-list.files(source.folder, full.names=T)
  all.dep<-lapply(all.files, function(x) depParse(x, human.list, object.list))
  all.dep<-do.call("rbind", all.dep)
  return(all.dep)
}

#collect results above by unqiue human or object word
collapseEntity<-function(sub.table){
  category<-sub.table$Category[1]
  name<-sub.table$Name[1]
  all.text<-paste(sub.table$Syntax, collapse=" ")
  entity<-c(category, name, all.text)
  return(entity)
}

collectTable<-function(syntax.table){
  unique.names<-unique(syntax.table$Name)
  sub.tables<-lapply(unique.names, function(x) syntax.table[which(syntax.table$Name==x),])
  collapsed.entities<-lapply(sub.tables, function(x) collapseEntity(x))
  entity.table<-do.call("rbind", collapsed.entities)
  return(entity.table)
}


