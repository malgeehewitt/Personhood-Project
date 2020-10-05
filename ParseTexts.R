parseTexts<-function(filename.list, source.folder="/scratch/groups/malgeehe/data/Chicago/CHICAGO_NOVEL_CORPUS", output.folder="/scratch/groups/malgeehe/data/Chicago/ParseNovels"){
  library(udpipe)
  input.filename.list<-paste(source.folder, filename.list, sep="/")
  output.filenames<-unlist(lapply(unlist(strsplit(filename.list, ".txt"))))
  output.filenames<-paste(output.filenames, "parsed.csv", sep="_")
  output.filenames<-paste(output.folder, output.filenames, sep="/")
  for(i in 1:length(input.filename.list)){
  	curr.file<-input.filename.list[i]
  	curr.output<-output.filenames[i]
  	input.file<-scan(curr.file, what='character', sep="\n", quiet=T)
  	input.file<-paste(input.file, collapse=" ")
  	full.parse<-udpipe(input.file, "english")
  	write.csv(full.parse, curr.output, row.names=F)
  }
}



ptm<-proc.time()
if (!require("Rmpi")) install.packages("Rmpi")
library(Rmpi)

# initialize an Rmpi environment
ns <- mpi.universe.size() - 1
mpi.spawn.Rslaves(nslaves=ns, needlog=TRUE)

chi.meta<-read.csv(file="/scratch/groups/malgeehe/data/Chicago/chicago_meta.csv")
all.files<-chi.meta$FILENAME
all.files<-sample(all.files, 8)
num.files<-length(all.files)
file.segment<-num.files/ns
seg.starts<-seq(1,num.files, by=file.segment)
seg.ends<-seg.starts[2:length(seg.stargs)]-1
seg.ends<-c(seg.ends, num.files)
file.vectors<-mapply(function(x,y) all.files[x:y], seg.starts, seg.ends, SIMPLIFY=F)
print(file.vectors)

#send function to all slaves
mpi.bcast.Robj2slave(parseTexts)

#scatter list to all slaves
mpi.scatter.Robj2slave(x<-file.vectors)


# all slaves execute this command
mpi.bcast.cmd(parseTexts(x))

print(proc.time()-ptm)

# close down the Rmpi environment
mpi.close.Rslaves(dellog = FALSE)
mpi.exit()