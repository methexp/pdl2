##--------------------------------------------------------------------------------------------------------------------------------------------
##						Pudel: Hilfsfunktionen.R
##--------------------------------------------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------------- 
#Funktion zum Bestimmen der vorhergehenden Reaktion
#	'Reaktion'	a factor
#	'Trial.Nr'	an integer specifying trial number
last.position<-function(Reaktion,Trial.Nr){
  
  last.position<-rep(NA,length(Reaktion))
  Reaktion<-as.integer(Reaktion)
  
  for (i in 1:length(Reaktion)){
    if(Trial.Nr[i]>1){
      last.position[i]<-Reaktion[i-1]
      }
    }
  last.position<-factor(last.position,levels=1:6,labels=c("1_","2_","3_","4_","5_","6_"))
  return(last.position)
  }


#------------------------------------------------------------------------------------------------------- 
#
# clevere Varianten von last.position()
#
#------------------------------------------------------------------------------------------------------- 
pdl.vR<-function(R,Trial.Nr){
  
  R<-as.integer(R)
  t<-as.integer(Trial.Nr)
  
  l<-length(R)
  
  vR<-rep(NA,l)
  
  vR[2:l]<-R[1:(l-1)]
  
  vR[t==1]<-NA
  
  return(vR)  
}

#------------------------------
pdl.vvR<-function(R,Trial.Nr){
  
  R<-as.integer(R)
  t<-as.integer(Trial.Nr)
  
  l<-length(R)
  
  vvR<-rep(NA,l)
  
  vvR[3:l]<-R[1:(l-2)]
  
  vvR[t<3]<-NA
  
  return(vvR)  
}

pdl.nR<-function(R,Trial.Nr,max.Trial){
  
  R<-as.integer(R)
  t<-as.integer(Trial.Nr)
  max.Trial<-as.integer(max.Trial)
  
  l<-length(R)
  
  nR<-rep(NA,l)
  
  nR[1:(l-1)]<-R[2:l]
  
  nR[t==max.Trial]<-NA
  
  return(nR)  
}

pdl.nnR<-function(R,Trial.Nr,max.Trial){
  
  R<-as.integer(R)
  t<-as.integer(Trial.Nr)
  max.Trial<-as.integer(max.Trial)
  
  l<-length(R)
  
  nR<-rep(NA,l)
  
  nR[1:(l-2)]<-R[3:l]
  
  nR[t>(max.Trial-2)]<-NA
  
  return(nR)  
}


#------------------------------------------------------------------------------------------------------- 
#Funktion bestimmt, ob frei generierte Sequenz in der Nachbefragung der tatsaechlichen Sequenz entspricht
#	'Reaktion'	a factor specifying reaction of the subject
#	'Trial.Nr'	an integer specifying trial number
#	'Sequenz'	eine Matrix mit allen Sequenzen, die in "./Sequenzen" liegen
#	'Vp.Nr'		Versuchspersonen-Nummer

korrekt.Nachbefragung<-function(Reaktion,Trial.Nr,Sequenz,Vp.Nr){

  Reaktion<-as.integer(Reaktion)
  Trial.Nr<-as.integer(Trial.Nr)
  Vp.Nr<-as.integer(Vp.Nr)

  korrekt<-rep(NA,length(Reaktion))
  
  for (i in 1:length(Reaktion)){
    n<-Vp.Nr[i]
    if(Trial.Nr[i]>1){
      if(is.na(sum(Sequenz[n,]))==FALSE){
	if(is.na(Reaktion[i])==FALSE){
	  for(x in 1:5){
	    if(Sequenz[n,x]==Reaktion[i-1]){
	      ifelse(Sequenz[n,x+1]==Reaktion[i],korrekt[i]<-1,korrekt[i]<-0)
	      }
	    }
	  if(Sequenz[n,6]==Reaktion[i-1]){
	    ifelse(Sequenz[n,1]==Reaktion[i],korrekt[i]<-1,korrekt[i]<-0)
	    }
	  } else {print(as.character("NAs erzeugt"))}
	  }
	}
      if(Trial.Nr[i]==1){
	if(is.na(sum(Sequenz[n,]))==FALSE){
	  if(is.na(Reaktion[i])==FALSE){
	    if(is.na(Reaktion[Vp.Nr==n][sum(Vp.Nr==n)])==FALSE){
	      for (x in 1:5){
		if(Sequenz[n,x]==Reaktion[Vp.Nr==n][sum(Vp.Nr==n)]){
		  ifelse(Sequenz[n,x+1]==Reaktion[i],korrekt[i]<-1,korrekt[i]<-0)
		  }
		}
	      if(Sequenz[n,6]==Reaktion[Vp.Nr==n][sum(Vp.Nr==n)]){
		ifelse(Sequenz[n,1]==Reaktion[i],korrekt[i]<-1,korrekt[i]<-0)
	      }
	    }
	  }
	}
      }
    }
  return(korrekt)
  }




#------------------------------------------------------------------------------------------------------- 
#Funktion zur Bestimmung, ob ein generierter uebergang einem der beiden ersten uebergaenge entspricht
#	'Reaktion'	a factor specifying reaction of the subject
#	'Trial.Nr'	an integer specifying trial number
#	'Sequenz'	eine Matrix mit allen Sequenzen, die in "./Sequenzen" liegen
#	'Vp.Nr'		Versuchspersonen-Nummer

mentioned.transition<-function(Reaktion,Trial.Nr,Sequenz,Vp.Nr){

  Reaktion<-as.integer(Reaktion)
  Trial.Nr<-as.integer(Trial.Nr)
  Vp.Nr<-as.integer(Vp.Nr)
  
  mentioned<-rep(0,length(Reaktion))
  
  for (i in 1:length(Reaktion)){
    n<-Vp.Nr[i]
    if(Trial.Nr[i]>1){
      for(x in 1:2){
	if(Sequenz[n,x]==Reaktion[i-1]){
	  ifelse(Sequenz[n,x+1]==Reaktion[i],mentioned[i]<-1,mentioned[i]<-0)
	  }
	}
      }
    }
  return(mentioned)
  }





#------------------------------------------------------------------------------------------------------- 
#''instruiert' von Christoph
#	'Reaktion'	a factor specifying reaction of the subject
#	'Trial.Nr'	an integer specifying trial number
#	'Sequenz'	eine Matrix mit allen Sequenzen, die in "./Sequenzen" liegen
#	'Vp.Nr'		Versuchspersonen-Nummer

instruiert<-function(vorige.Reaktion,Trial.Nr,Sequenz,Vp.Nr){

  vorige.Reaktion<-as.integer(vorige.Reaktion)
  Trial.Nr<-as.integer(Trial.Nr)
  Vp.Nr<-as.integer(Vp.Nr)
  
  instruiert<-rep(1,length(vorige.Reaktion))
  
  for (i in 1:length(vorige.Reaktion)){
    n<-Vp.Nr[i]
    if(Trial.Nr[i]>1){
      if(vorige.Reaktion[i]==(Sequenz[n,6]|Sequenz[n,3])){
	#rand-instruiert
	instruiert[i]<-3
	}
      if(vorige.Reaktion[i]==(Sequenz[n,1]|Sequenz[n,2])){
	#instruiert
	instruiert[i]<-2
	}
      }
    }
  return(instruiert)
  }


#------------------------------------------------------------------------------------------------------- 
##'instruiert' von Christoph (generalisiert)
#	'Reaktion'	a factor specifying reaction of the subject
#	'Trial.Nr'	an integer specifying trial number
#	'Sequenz'	eine Matrix mit allen Sequenzen, die in "./Sequenzen" liegen
#	'Vp.Nr'		Versuchspersonen-Nummer

instruiert.gen<-function(vorige.Reaktion,Anzahl.instruiert,Sequenz,Vp.Nr){

  vorige.Reaktion<-as.integer(vorige.Reaktion)
  Vp.Nr<-as.integer(Vp.Nr)
  
  instruiert<-rep(NA,length(vorige.Reaktion))
  #1=nicht instruiert, 2=instruiert,3=rand-instruiert
  for (i in 1:length(vorige.Reaktion)){
    n<-Vp.Nr[i]
    if(is.na(vorige.Reaktion[i])==FALSE){
      instruiert[i]<-1
      if(Anzahl.instruiert[i]=="Expl.Two"){
	#rand-instruiert
	if(vorige.Reaktion[i]==Sequenz[n,6]){
	  instruiert[i]<-3
	  }
	if(vorige.Reaktion[i]==Sequenz[n,3]){
	  instruiert[i]<-3
	  }	  
	#instruiert
	if(vorige.Reaktion[i]==Sequenz[n,1]){
	  instruiert[i]<-2
	  }
	if(vorige.Reaktion[i]==Sequenz[n,2]){
	  instruiert[i]<-2
	  }
	}
      if(Anzahl.instruiert[i]=="Expl.One"){
	#rand-instruiert
	if(vorige.Reaktion[i]==Sequenz[n,6]){
	  instruiert[i]<-3
	  }
	if(vorige.Reaktion[i]==Sequenz[n,2]){
	  instruiert[i]<-3
	  }	  
	#instruiert
	if(vorige.Reaktion[i]==Sequenz[n,1]){
	  instruiert[i]<-2
	  }
	}	
      }
    }
  return(instruiert)
  }



#------------------------------------------------------------------------------------------------------- 
#Stelle in der Sequenz


Sequenz.Stelle<-function(Reaktion,Trial.Nr,Sequenz,Vp.Nr){

  Reaktion<-as.integer(Reaktion)
  Trial.Nr<-as.integer(Trial.Nr)
  Vp.Nr<-as.integer(Vp.Nr)
  
  Stelle<-rep(0,length(Reaktion))
  
  for (i in 1:length(Reaktion)){
    n<-Vp.Nr[i]
    if(is.na(Reaktion[i])==FALSE){
      for(x in 1:6){
	if(is.na(Sequenz[n,x])==FALSE){
	  if(Sequenz[n,x]==Reaktion[i]){
	    Stelle[i]<-x
	    }
	  }
	}
      }
    }
  return(Stelle)
  }




#------------------------------------------------------------------------------------------------------- 
#Funktion zur Bestimmung, ob eine generierte Position in den Instruktionen erwaehnt wurde
#	'Reaktion'	a factor specifying reaction of the subject
#	'Trial.Nr'	an integer specifying trial number
#	'Sequenz'	eine Matrix mit allen Sequenzen, die in "./Sequenzen" liegen
#	'Vp.Nr'		Versuchspersonen-Nummer

mentioned.position<-function(Reaktion,Trial.Nr,Sequenz,Vp.Nr){

  Reaktion<-as.integer(Reaktion)
  Trial.Nr<-as.integer(Trial.Nr)
  Vp.Nr<-as.integer(Vp.Nr)
  
  mentioned<-rep(NA,length(Reaktion))
  
  for (i in 1:length(Reaktion)){
    n<-Vp.Nr[i]
    Test<-rep(NA,3)

    for(x in 1:3){
      ifelse(Sequenz[n,x]==Reaktion[i],Test[x]<-1,Test[x]<-0)
      }
    mentioned[i]<-as.integer(sum(Test)==1)

    }
  return(mentioned)
  }






  


#------------------------------------------------------------------------------------------------------- 
#Funktion zum Bestimmen von Trillern
#	'Reaktion'	a factor specifying reaction of the subject
#	'Trial.Nr'	an integer specifying trial number
#	'Generierung'	a factor specifying type of generation task ('Gen.A' vs. 'Gen.B')
#	'Vp.Nr'		Versuchspersonen-Nummer
#	'Cue5'		a vector of last presented cue position
#	'Cue4'		a vector of the position presented before last presented cue

Triller.bestimmen<-function(Reaktion,Generierung,Cue4,Cue5,Trial.Nr){

  Reaktion<-as.integer(Reaktion)
  Trial.Nr<-as.integer(Trial.Nr)
  Cue4<-as.integer(Cue4)
  Cue5<-as.integer(Cue5)
  
  Triller<-rep(NA,length(Reaktion))
  warnings<-0
  
  for (i in 1:length(Reaktion)){
    if(is.na(Reaktion[i])==FALSE){
      if(Generierung[i]=="Gen.A"){
	if(Trial.Nr[i]>2){
	  if(Reaktion[i-1]!=Reaktion[i]){
	    ifelse(Reaktion[i-2]==Reaktion[i],Triller[i]<-1,Triller[i]<-0)
	    }
	  }
	}
      if(Generierung[i]=="Gen.B"){
	if(Cue5[i]!=Reaktion[i]){
	  ifelse(Cue4[i]==Reaktion[i],Triller[i]<-1,Triller[i]<-0)
	  }
	}
      } else{warnings<-warnings+1}
    }
  if(warnings>0){
    print("NAs in 'Reaktion'. NAs erzeugt.")
    }
  return(Triller)
  }



#---------------------------------------------------------------------------------------------------------------------------------------------------
#Funktion bestimmt, ob die Position der Reaktion auch als Cue praesentiert worden ist (bzw. waere!!!)
was.cue<-function(data,Reaktion,cue.length,cue1,cue2,cue3,cue4,cue5){
  
  cue1<-as.integer(data[[as.character(cue1)]])
  cue2<-as.integer(data[[as.character(cue2)]])
  cue3<-as.integer(data[[as.character(cue3)]])
  cue4<-as.integer(data[[as.character(cue4)]])
  cue5<-as.integer(data[[as.character(cue5)]])
  cue.length<-as.integer(data[[as.character(cue.length)]])
  Reaktion<-as.integer(data[[as.character(Reaktion)]])
  
  was.cue<-rep(NA,nrow(data))
  
  for (i in 1:nrow(data)){
    switch(cue.length[i],
      was.cue[i]<-NA,
      was.cue[i]<-NA,
      was.cue[i]<-as.integer((Reaktion[i]==cue5[i])|(Reaktion[i]==cue4[i])|(Reaktion[i]==cue3[i])),
      was.cue[i]<-as.integer((Reaktion[i]==cue5[i])|(Reaktion[i]==cue4[i])|(Reaktion[i]==cue3[i])|(Reaktion[i]==cue2[i])),
      was.cue[i]<-as.integer((Reaktion[i]==cue5[i])|(Reaktion[i]==cue4[i])|(Reaktion[i]==cue3[i])|(Reaktion[i]==cue2[i])|(Reaktion[i]==cue1[i]))
      )
    }
  return(was.cue)
  }



  

  
runs.counter<-function(data,Deviante,Vp.Nr){
  Deviante<-as.integer(data[[as.character(Deviante)]])
  Vp.Nr<-as.integer(data[[as.character(Vp.Nr)]])
  
  n.obs<-length(Deviante)
  run.nr<-array(NA,dim=c(n.obs))
  runs.counter<-array(1,dim=max(Vp.Nr))
  run.part<-array(NA,dim=c(n.obs))
  
  for (i in 2:n.obs){
    if(is.na(Deviante[i])==FALSE){
      n<-Vp.Nr[i]
      if(is.na(Deviante[i-1])){
	run.part[i]<-1
	}
      if(is.na(Deviante[i-1])==FALSE){
	
	if(Deviante[i]!=Deviante[i-1]){
	  runs.counter[n]<-runs.counter[n]+1
	  run.part[i]<-1
	  }
	if(Deviante[i]==Deviante[i-1]){
	  run.part[i]<-run.part[i-1]+1
	  }
	}
      run.nr[i]<-runs.counter[n]
      }
    }
  return(list("run.nr"=run.nr,"run.part"=run.part))
  }

	

#---------------------------------------------------------------------------------------------------------------------------------------------------
#'Kriterium' aus 'Sequenzen'
# works for sequences of six locations

Sequenz.to.Kriterium<-function(Sequenzen){

  Kriterium<-array(NA,dim=dim(Sequenzen))
  
  for (n in 1:nrow(Sequenzen)){
    if(is.na(sum(Sequenzen[n,]))==FALSE){
      for (i in 1:ncol(Sequenzen)){
	for (x in 1:5){
	  if(Sequenzen[n,x]==i){
	    Kriterium[n,i]<-Sequenzen[n,x+1]
	    }
	  }
	if(Sequenzen[n,6]==i){
	  Kriterium[n,i]<-Sequenzen[n,1]
	  }
	}
      }
    }
  return(Kriterium)
  }




six.pack<-function(Reaktion,Trial.Nr){

  Reaktion<-as.integer(Reaktion)
  Trial.Nr<-as.integer(Trial.Nr)
  
  six.pack<-rep(NA,length(Reaktion))
  
  for(i in 1:length(Reaktion)){
    if(Trial.Nr[i]>5){
      six.pack[i]<-as.integer(sum(sort(Reaktion[(i-5):i])==1:6)==6)
      }
    }
  return(six.pack)
  }
  


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#kodiert, ob ein uebergang blosz aufsteigend ist, 1=aufsteigend, 0=etwas anderes 
ascending<-function(vorige.Reaktion,Reaktion){

  vorige.Reaktion<-as.integer(vorige.Reaktion)
  Reaktion<-as.integer(Reaktion)
  
  ascending<-rep(NA,length(Reaktion))

  for (i in 1:length(Reaktion)){
    if(is.na(vorige.Reaktion[i])==FALSE){
      ascending[i]<-as.integer(vorige.Reaktion[i]+1==Reaktion[i])
      if(vorige.Reaktion[i]==6){
	ascending[i]<-as.integer(Reaktion[i]==1)
	}
      }
    }
  return(ascending)
  }



#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#kodiert, ob ein uebergang blosz absteigend ist, 1=absteigend, 0=etwas anderes 
descending<-function(vorige.Reaktion,Reaktion){

  vorige.Reaktion<-as.integer(vorige.Reaktion)
  Reaktion<-as.integer(Reaktion)
  
  descending<-rep(NA,length(Reaktion))

  for (i in 1:length(Reaktion)){
    if(is.na(vorige.Reaktion[i])==FALSE){
      descending[i]<-as.integer(vorige.Reaktion[i]-1==Reaktion[i])
      if(vorige.Reaktion[i]==1){
	descending[i]<-as.integer(Reaktion[i]==6)
	}
      }
    }
  return(descending)
  }


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#kodiert, ob ein uebergang blosz Spiegelung ist, 1=Spiegelung, 0=etwas anderes 
mirrored<-function(vorige.Reaktion,Reaktion){

  vorige.Reaktion<-as.integer(vorige.Reaktion)
  Reaktion<-as.integer(Reaktion)
  
  mirrored<-rep(NA,length(Reaktion))

  for (i in 1:length(Reaktion)){
    if(is.na(vorige.Reaktion[i])==FALSE){
      mirrored[i]<-as.integer((vorige.Reaktion[i]+Reaktion[i])==7)
      }
    }
  return(mirrored)
  }


      
## Unabhaengig vom tatsaechlich generierten uebergang, hat die VPn fuer diese 'vorige.Reaktion' in der Nachbefragung die richtige Loesung gewusst?
Sequenzwissen.in.Nachbefragung<-function(vorige.Reaktion,Vp.Nr,Kriterium,Kriterium.emp){

  vorige.Reaktion<-as.integer(vorige.Reaktion)
  Vp.Nr<-as.integer(Vp.Nr)
  Sequenzwissen<-array(NA,dim=length(vorige.Reaktion))

  for (i in 1:length(vorige.Reaktion)){
    if(is.na(vorige.Reaktion[i])==FALSE){
      n<-Vp.Nr[i]
      Sequenzwissen[i]<-as.integer(Kriterium[n,vorige.Reaktion[i]]==Kriterium.emp[n,vorige.Reaktion[i]])
      }
    }
  return(Sequenzwissen)
  }
  







seq.abs<-function(Reaktion,vorige.Reaktion,Trial.Nr,Vp.Nr,Sequenz){
  
  seq.abs<-rep(NA,length(Reaktion))
  Reaktion<-as.integer(Reaktion)
  vorige.Reaktion<-as.integer(vorige.Reaktion)
  Vp.Nr<-as.integer(Vp.Nr)
  Trial.Nr<-as.integer(Trial.Nr)

  for (i in 1:length(Reaktion)){
    if(is.na(vorige.Reaktion[i])==FALSE){
      n<-Vp.Nr[i]
      if(is.na(sum(Sequenz[n,]))==FALSE){
      
      for (j in 1:5){
	if(vorige.Reaktion[i]==Sequenz[n,j]){
	  if(Reaktion[i]==Sequenz[n,j+1]){
	    seq.abs[i]<-j
	    }
	  }
	}
      if(vorige.Reaktion[i]==Sequenz[n,6]){
	if(Reaktion[i]==Sequenz[n,1]){
	  seq.abs[i]<-6
	  }
	}
      }
    }
    }
  return(seq.abs)
  }


############################################################
## formal korrekte Antwort erzeugen

perfect.person<-function(vorige.Reaktion,Vp.Nr,Kriterium){
  
  vorige.Reaktion<-as.integer(vorige.Reaktion)
  Vp.Nr<-as.integer(Vp.Nr)
  Reaktion<-rep(NA,length(vorige.Reaktion))
  
  for (i in 1:length(vorige.Reaktion)){
    n<-Vp.Nr[i]
    Reaktion[i]<-Kriterium[n,vorige.Reaktion[i]]
    }
  return(Reaktion)
  }


 
 
########################################################
## markiere uebergaenge, fuer die in Lernphase RT.Drop diagnostiziert
Kodiere.RT.Drops<-function(Vp.Nr,SEQ_ABS,Daten.tmp){
  
  Vp.Nr<-as.integer(Vp.Nr)
  SEQ_ABS<-as.integer(SEQ_ABS)
  RT.Drop<-rep(0,length(Vp.Nr))

  for (i in 1:length(Vp.Nr)){
  n<-Vp.Nr[i]
  seq<-SEQ_ABS[i]
  
  if(is.na(Daten.tmp[n,seq])==FALSE){
    RT.Drop[i]<-1
    }
  }
  return(RT.Drop)
  }






## bestimmt, ob eine Reaktion die erste Deviante nach einer Sequenz ist
erste.Deviante<-function(Runs){
   
  erste.Deviante<-rep(0,length(Runs))
  
  for (i in 2:length(Runs)){
    if(is.na(as.integer(Runs[i-1])+as.integer(Runs[i]))==FALSE){
      if(Runs[i]=="Deviante"){
	if(Runs[i-1]=="Sequenz"){
	  erste.Deviante[i]<-1
	  }
	}
      }
    }
  erste.Deviante<-factor(erste.Deviante,levels=c(0,1),labels=c("is.not","is"))
  
  return(erste.Deviante)
  }





##########################################
## Ist der uebergang auch ein Handwechsel

Handwechsel<-function(vorige.Reaktion,Reaktion){

  vorige.Reaktion<-as.integer(vorige.Reaktion)
  Reaktion<-as.integer(Reaktion)
  
  n.obs<-length(vorige.Reaktion)
  
  Handwechsel<-rep(NA,n.obs)
  
  for (i in 1:n.obs){
    if(!is.na(vorige.Reaktion[i])){
      Handwechsel[i]<-as.integer(((vorige.Reaktion[i]>3)&(Reaktion[i]<4))|((vorige.Reaktion[i]<4)&(Reaktion[i]>3)))
      }
    }
  return(Handwechsel)
  }




##-------------------------------------------------------------------------------------------------------------------------------------------------------
##
## Funktion bestimmt, ob die generierte Reaktion eine der instruierten Zielpositionen ist
##
##-------------------------------------------------------------------------------------------------------------------------------------------------------
Ziel.instruiert<-function(Reaktion,Sequenzen,Vp.Nr){
  
  Reaktion<-as.integer(Reaktion)
  Vp.Nr<-as.integer(Vp.Nr)
  instruiert<-rep(NA,length(Reaktion))
  
  for (i in 1:length(Reaktion)){
    n<-Vp.Nr[i]
    instruiert[i]<-as.integer(Reaktion[i]==Sequenzen[n,2])
  }
  return(instruiert)
}

##-------------------------------------------------------------------------------------------------------------------------------------------------------
##
## Funktion bestimmt, ob die generierte Reaktion die instruierte Startposition ist
##
##-------------------------------------------------------------------------------------------------------------------------------------------------------
Start.instruiert<-function(Reaktion,Sequenzen,Vp.Nr){
  
  Reaktion<-as.integer(Reaktion)
  Vp.Nr<-as.integer(Vp.Nr)
  instruiert<-rep(NA,length(Reaktion))
  
  for (i in 1:length(Reaktion)){
    n<-Vp.Nr[i]
    instruiert[i]<-as.integer(Reaktion[i]==Sequenzen[n,1])
  }
  return(instruiert)
}

##-------------------------------------------------------------------------------------------------------------------------------------------------------
##
## "kritischer Triller" ist der uebergang nach uinstruiertem uebergang, wenn Triller
##
##-------------------------------------------------------------------------------------------------------------------------------------------------------
Triller.krit<-function(korrekt,instruiert,Triller,Trial.Nr){
  
  #korrekt<-as.integer(korrekt)
  #Triller<-as.integer(Triller)
  
  Triller.krit<-rep(NA,length(Triller))
  
  for (i in 1:length(Triller.krit)){
    if(Trial.Nr[i]>2){
      Triller.krit[i]<-as.integer((Triller[i]==1)*(korrekt[i-1]==1)*(instruiert[i-1]=="instruiert"))
    }
  }
  return(Triller.krit)
}

##-------------------------------------------------------------------------------------------------------------------------------------------------------
##
## bestimmt fuer jede Reaktion die 'absolute' Position in der Sequenz 
## (Ist es die erste, zweite, dritte, etc. Position der Sequenz???)
##
##-------------------------------------------------------------------------------------------------------------------------------------------------------
Sequenz_i<-function(Reaktion,Vp.Nr,Sequenzen){
  
  Reaktion<-as.integer(Reaktion)
  seq<-rep(NA,length(Reaktion))
  
  for (i in 1:length(Reaktion)){
    if(!is.na(Reaktion[i])){
      n<-Vp.Nr[i]
      for (j in 1:6){
        if(!is.na(Sequenzen[n,j])&&Sequenzen[n,j]==Reaktion[i]){
          seq[i]<-j
        }
      }
    }
  }
  return(seq)
}

##-------------------------------------------------------------------------------------------------------------------------------------------------------
##
## RT-Plots fuer die Lernphase
##
##-------------------------------------------------------------------------------------------------------------------------------------------------------
pdl.rt.plots<-function(data,id,dv,factors,ylim=c(0,5000),...){

  tmp<-aggregate(formula=formula(paste(c(dv,"~",id,"+",paste(factors,collapse="+")),collapse="")),data=data,FUN=mean)
  
  tmp.m <-aggregate(formula=formula(paste(c(dv,"~",paste(factors,collapse="+")),collapse="")),data=tmp,FUN=mean)
  tmp.sd<-aggregate(formula=formula(paste(c(dv,"~",paste(factors,collapse="+")),collapse="")),data=tmp,FUN=sd)
  tmp.n<-
  
  l1<-levels(tmp[[factors[1]]])
  l2<-levels(tmp[[factors[2]]])
  
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[1]]
  plot(x=l1,y=y,type="l",col="green",ylim=ylim,xlab=factors[1],ylab="M(RT)+-SD(RT) [ms]",sub=paste(c("green==",l2[1],", red==",l2[2]),collapse=""),...)
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[1]]+tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[1]]
  lines(x=l1,y=y,col="green",lty="dotted")
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[1]]-tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[1]]
  lines(x=l1,y=y,col="green",lty="dotted")
  
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[2]]
  lines(x=l1,y=y,col="red") 
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[2]]+tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[2]]
  lines(x=l1,y=y,col="red",lty="dotted")
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[2]]-tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[2]]
  lines(x=l1,y=y,col="red",lty="dotted")
  
  if (nlevels(tmp[[factors[2]]])==3){
    y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[3]]
    lines(x=l1,y=y,col="black") 
    y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[3]]+tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[3]]
    lines(x=l1,y=y,col="black",lty="dotted")
    y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[3]]-tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[3]]
    lines(x=l1,y=y,col="black",lty="dotted")
    
  }
}
##-------------------------------------------------------------------------------------------------------------------------------------------------------
##
## Lag zwischen gleichen Positionen
##
##-------------------------------------------------------------------------------------------------------------------------------------------------------

Position.Lag<-function(Position,Trial.Nr,Vp.Nr,Block.Nr){
  
  Position<-as.integer(Position)
  Vp.Nr<-as.integer(Vp.Nr)
  Block.Nr<-as.integer(Block.Nr)
  Trial.Nr<-as.integer(Trial.Nr)
  
  Lag<-rep(NA,length(Position))
  
  test<-array(NA,dim=c(max(Vp.Nr,na.rm=TRUE),max(Block.Nr,na.rm=TRUE),max(Position,na.rm=TRUE)))
  
  for (i in 1:length(Position)){
    
    n<-Vp.Nr[i]
    b<-Block.Nr[i]
    p<-Position[i]
    
    Lag[i]<-Trial.Nr[i]-test[n,b,p]
    test[n,b,p]<-Trial.Nr[i]
  }
  return(Lag)
}

direkte.Wiederholung<-function(vorige.Reaktion,Reaktion){
  
  Reaktion<-as.integer(Reaktion)
  vorige.Reaktion<-as.integer(vorige.Reaktion)
  
  direkte.Wiederholung<-as.integer(Reaktion==vorige.Reaktion)
  
  return(direkte.Wiederholung)
}

##-------------------------------------------------------------------------------------------------------------------------------------------------------
##
## RT-Plots fuer die Lernphase
##
##-------------------------------------------------------------------------------------------------------------------------------------------------------
pdl.rt.plots<-function(data,id,dv,factors,ylim=c(0,5000),dispersion="sd",...){
  
  tmp<-aggregate(formula=formula(paste(c(dv,"~",id,"+",paste(factors,collapse="+")),collapse="")),data=data,FUN=mean)
  
  tmp.m <-aggregate(formula=formula(paste(c(dv,"~",paste(factors,collapse="+")),collapse="")),data=tmp,FUN=mean)
  tmp.sd<-aggregate(formula=formula(paste(c(dv,"~",paste(factors,collapse="+")),collapse="")),data=tmp,FUN=sd)
  
  
  if(dispersion=="CI"){
    tmp.sd<-aggregate(formula=formula(paste(c(dv,"~",paste(factors,collapse="+")),collapse="")),data=tmp,FUN=confidence)
  }
  
  l1<-levels(tmp[[factors[1]]])
  l2<-levels(tmp[[factors[2]]])
  
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[1]]
  plot(x=l1,y=y,type="l",col="green",ylim=ylim,xlab=factors[1],ylab="Reaktionszeit [ms]",sub=paste(c("green==",l2[1],", red==",l2[2]),collapse=""),...)
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[1]]+tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[1]]
  lines(x=l1,y=y,col="green",lty="dotted")
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[1]]-tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[1]]
  lines(x=l1,y=y,col="green",lty="dotted")
  
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[2]]
  lines(x=l1,y=y,col="red") 
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[2]]+tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[2]]
  lines(x=l1,y=y,col="red",lty="dotted")
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[2]]-tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[2]]
  lines(x=l1,y=y,col="red",lty="dotted")
  
  if (nlevels(tmp[[factors[2]]])==3){
    y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[3]]
    lines(x=l1,y=y,col="black") 
    y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[3]]+tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[3]]
    lines(x=l1,y=y,col="black",lty="dotted")
    y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[3]]-tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[3]]
    lines(x=l1,y=y,col="black",lty="dotted")
    
  }
}



confidence<-function(x){
  y<-sd(x)/sqrt(sum(!is.na(x)))
}


pdl.rt.Katha<-function(data,id,dv,factors,ylim=c(0,5000),dispersion="SD",...){
  
  tmp<-aggregate(formula=formula(paste(c(dv,"~",id,"+",paste(factors,collapse="+")),collapse="")),data=data,FUN=mean)
  
  tmp.m <-aggregate(formula=formula(paste(c(dv,"~",paste(factors,collapse="+")),collapse="")),data=tmp,FUN=mean)
  tmp.sd<-aggregate(formula=formula(paste(c(dv,"~",paste(factors,collapse="+")),collapse="")),data=tmp,FUN=sd)
  
  
  if(dispersion=="CI"){
    tmp.sd<-aggregate(formula=formula(paste(c(dv,"~",paste(factors,collapse="+")),collapse="")),data=tmp,FUN=confidence)
  }
  
  l1<-levels(tmp[[factors[1]]])
  l2<-levels(tmp[[factors[2]]])
  
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[1]]
  plot(x=l1,y=y,type="b",col="black",ylim=ylim,xlab="Blocknummer je Material",ylab="Reaktionszeit [ms]",pch=1,...)
  lines(x=l1,y=y,col="black")
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[1]]+tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[1]]
  lines(x=l1,y=y,col="black",lty="dotted")
  points(x=l1,y=y,col="black",pch=1)
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[1]]-tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[1]]
  lines(x=l1,y=y,col="black",lty="dotted")
  points(x=l1,y=y,col="black",pch=1)
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[2]]
  lines(x=l1,y=y,col="black") 
  points(x=l1,y=y,col="black",pch=4)
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[2]]+tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[2]]
  lines(x=l1,y=y,col="black",lty="dotted")
  points(x=l1,y=y,col="black",pch=4)
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[2]]-tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[2]]
  lines(x=l1,y=y,col="black",lty="dotted")
  points(x=l1,y=y,col="black",pch=4)
  
}

#pdl.rt.Katha(data=Daten.Lrn,id="Vp.Nr",dv="Reaktionszeit",factors=c("Block_Shape","Material"),ylim=c(730,810),dispersion="CI")

pdl.rt.bw<-function(data,id,dv,factors, na.rm=TRUE, ylim=c(0,5000), dispersion="SD", xlab="block number", ylab="Mean RTs [ms][95% CI]"){
  
  data[[factors[1]]]<-as.factor(data[[factors[1]]])
  data[[factors[2]]]<-as.factor(data[[factors[2]]])
  
  data[[factors[1]]]<-droplevels(data[[factors[1]]])
  data[[factors[2]]]<-droplevels(data[[factors[2]]])
  
  
  tmp<-aggregate(formula=formula(paste(c(dv,"~",id,"+",paste(factors,collapse="+")),collapse="")),data=data,FUN=mean,na.rm=na.rm)
  
  tmp.m <-aggregate(formula=formula(paste(c(dv,"~",paste(factors,collapse="+")),collapse="")),data=tmp,FUN=mean)
  tmp.sd<-aggregate(formula=formula(paste(c(dv,"~",paste(factors,collapse="+")),collapse="")),data=tmp,FUN=sd)
  
  
  if(dispersion=="CI"){
    tmp.sd<-aggregate(formula=formula(paste(c(dv,"~",paste(factors,collapse="+")),collapse="")),data=tmp,FUN=confidence)
  }
  
  l1<-as.integer(levels(tmp[[factors[1]]]))
  l2<-levels(tmp[[factors[2]]])
  
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[1]]
  plot(x=l1,y=y,type="b",col="black", ylim=ylim, xlab=xlab, ylab=ylab, pch=1)
  lines(x=l1,y=y,col="black")
  
  upper<-tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[1]]
  error.bar(x=l1,y=y,upper=upper)  
  
  y<-tmp.m[[dv]][tmp.m[[factors[2]]]==l2[2]]
  lines(x=l1,y=y,col="black",lty="dashed") 
  points(x=l1,y=y,col="black",pch=4)
  upper<-tmp.sd[[dv]][tmp.sd[[factors[2]]]==l2[2]]
  error.bar(x=l1,y=y,upper=upper)  
}

subjs.from.data<-function(path="./path.to.raw.data",ending=".dat"){

  subjs<-sort(as.integer(sub(x=grep(list.files(path=path),pattern=ending,value=TRUE),pattern=ending,replacement="")))
  return(subjs)
}

excluded.id<-function(id,excludes=c()){

  id<-as.integer(id)
  excludes<-as.integer(excludes)
  
  excluded.id<-as.integer(id %in% excludes)
  
  return(excluded.id)
}



