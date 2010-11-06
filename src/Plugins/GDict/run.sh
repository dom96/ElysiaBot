# the -i.. appends .. to the searchpath 
ghc --make -i.. -fforce-recomp GDict.hs && ./GDict
