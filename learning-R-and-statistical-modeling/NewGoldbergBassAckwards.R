#New GoldbergBassAckwards 

#####STEP 0: LIBRARIES
library(stats)
library(psych)

#First one is an adaptation of the bassackward code from the psych package. The input is a correlation matrix, and it's currently only set up to do orthogonal PCA

##CORRELATION
baselineDataNumeric <- select(baselineData, c(10:102, 106:114))
oneyearDataNumeric <- select(oneyearData, c(10:102, 106:114)) 
twoyearDataNumeric <- select(twoyearData, c(10:102, 106:114))

baselineCor <- cor(baselineDataNumeric)
oneyearCor <- cor(oneyearDataNumeric)
twoyearCor <- cor(twoyearDataNumeric)


######STEP 2: FUNCTIONS
extended_bassAckward <- function (r, num.comp = 1, fm = "pca")
{
  comp.corr <- list()
  cong <- list()
  comp.load <- list()
  pcas <- list()
  
  for (c in 1:num.comp) {
    #this is running the pcas at each level
    pcas[[c]] <- psych::pca(r, nfactors = c, rotate = "geominT")
    comp <- psych::pca(r, nfactors = c, rotate = "geominT")
    colnames(comp$loadings) <-
      paste0(letters[c], 1:ncol(comp$loadings)) #each level gets a different letter as a prefix
    comp.load[[c]] <- fa.sort(comp$loadings)
    comp.weights <- comp$weights
    colnames(comp.weights) <-
      paste0(letters[c], 1:ncol(comp.weights))
    unsort.loadings <- comp$loadings
    colnames(unsort.loadings) <-
      paste0(letters[c], 1:ncol(unsort.loadings))
    
    if (c > 1) {
      for (i in 1:(c - 1)) {
        colnames(pcas[[i]]$weights) <-
          paste0(letters[i], 1:ncol(pcas[[i]]$weights)) #names weights matrices sequentially
        colnames(pcas[[i]]$loadings) <-
          paste0(letters[i], 1:ncol(pcas[[i]]$loadings)) #names weights matrices sequentially
        comp.corr[[length(comp.corr) + 1]] <-
          t(pcas[[i]]$weights) %*% r %*%  comp.weights
        cong[[length(cong) + 1]] <-
          psych::factor.congruence(pcas[[i]]$loadings, unsort.loadings)
      }
    }
    
  }
  
  result <- list(
    comp.corr = comp.corr,
    pcas = pcas,
    cong = cong,
    r = r,
    comp.load = comp.load)
  
  return(result)
}



corr_chase_path <- function (comp.corr, component = "levelnum")
{
  chased_levels <- vector()
  chased_to_level <- vector()
  chased_to <- list()
  sub_revcomp.corr <- list()
  
  for (i in (length(comp.corr):1))
  {
    if (component %in% colnames(comp.corr[[i]]))
      #if the component we're interested is in the matrix
      chased_levels[[i]] <-
        (max(comp.corr[[i]][, component]) >= .9) #tell me if the maximum component correlation for the relevant column is >=.9
  }
  
  revcomp.corr <-
    rev(comp.corr) #reverse order of comp.corr to index below
  component_level <-
    (length(chased_levels[!is.na(chased_levels)]) + 1) #level of current component (based on number of upward comparison matrices +1)
  chased_levels <-
    rev(chased_levels) #reverses order, so looking at lowest levels of hierarchy first
  
  if (any(chased_levels, na.rm = TRUE))
  {
    chased_levels <-
      (which.min(chased_levels)) - 1 #counts number of consecutive true values before first false
  }
  else {
    chased_levels <- 0
  }
  
  chased_to_level <- (component_level - chased_levels)
  
  if (chased_levels == 0)
  {
    chased_to <- "null"
  } #if no trues
  else {
    #isolate block of matrices in revcomp.corr relevant to component
    #end range
    end_comp.corr <- ((component_level * (component_level - 1) / 2))
    #start range
    start_comp.corr <- (end_comp.corr - (component_level - 2))
    sub_comp.corr <-
      comp.corr[start_comp.corr:end_comp.corr] #subset of matrices
    
    chased_to <-
      rownames(as.data.frame(which.max(sub_comp.corr[[chased_to_level]][, component]))) #name of component chased to
    
  }
  if ((component == "b1") &
      (max(comp.corr[[1]][, "b1"]) >= .9))
    #need to do the [[1]] level manually
  {
    chased_to <- "a1"
  }
  if ((component == "b2") & (max(comp.corr[[1]][, "b2"]) >= .9))
  {
    chased_to <- "a1"
  }
  
  result <- list(component, chased_to)
  result <- paste(result, sep = " ", collapse = "--")
  
  return(result)
}



cong_chase_path <-
  function (cong, component = "levelnum")
    #this is the function that shows the congruence coefficient paths chased with >.95
  {
    chased_levels <- vector()
    chased_to_level <- vector()
    chased_to <- list()
    sub_revcong <- list()
    
    for (i in (length(cong):1))
    {
      if (component %in% colnames(cong[[i]]))
        #if the component we're interested is in the matrix
        chased_levels[[i]] <-
          (max(cong[[i]][, component]) > .95) #tell me if the maximum component correlation for the relevant column is >.95
    }
    
    revcong <- rev(cong) #reverse order of cong to index below
    component_level <-
      (length(chased_levels[!is.na(chased_levels)]) + 1) #level of current component (based on number of upward comparison matrices +1)
    chased_levels <-
      rev(chased_levels) #reverses order, so looking at lowest levels of hierarchy first
    
    if (any(chased_levels, na.rm = TRUE))
    {
      chased_levels <-
        (which.min(chased_levels)) - 1 #counts number of consecutive true values before first false
    }
    else {
      chased_levels <- 0
    }
    
    chased_to_level <- (component_level - chased_levels)
    
    if (chased_levels == 0)
    {
      chased_to <- "null"
    } #if no trues
    else {
      #isolate block of matrices in revcomp.corr relevant to component
      #end range
      end_cong <- ((component_level * (component_level - 1) / 2))
      #start range
      start_cong <- (end_cong - (component_level - 2))
      
      sub_cong <- cong[start_cong:end_cong] #subset of matrices
      
      chased_to <-
        rownames(as.data.frame(which.max(sub_cong[[chased_to_level]][, component]))) #name of component chased to
      
    }
    if ((component == "b1") &
        (max(cong[[1]][, "b1"]) > .95))
      #need to do the [[1]] level manually
    {
      chased_to <- "a1"
    }
    if ((component == "b2") & (max(cong[[1]][, "b2"]) > .95))
    {
      chased_to <- "a1"
    }
    
    result <- list(component, chased_to)
    result <- paste(result, sep = " ", collapse = "--")
    
    return(result)
  }


redundant_path <-
  function (comp.corr, cong, last_component = "levelnum")
  {
    chase <- list()
    congruence <- list()
    largest <- list()
    cross <- list()
    output <- list()
    mat <- matrix(data = 1:625,
                  nrow = 25,
                  ncol = 25)
    
    for (i in 1:25)
    {
      mat[, i] <- i
    }
    mat[upper.tri(mat)] <- NA
    matlett <- matrix(data = 1:625,
                      nrow = 25,
                      ncol = 25)
    
    for (i in 1:25)
    {
      matlett[i, ] <- i
    }
    
    matlett[] <- letters[as.matrix(matlett)]
    matlett[upper.tri(matlett)] <- NA
    comp_list <-
      paste0(na.omit(as.vector(t(matlett))), na.omit(as.vector(t(mat)))) #list of all possible component names up to 25 hierarchical levels
    
    for (i in 2:(which(comp_list == last_component)[[1]]))
    {
      chase[i] <- corr_chase_path(comp.corr, comp_list[i])
      congruence[i] <- cong_chase_path(cong, comp_list[i])
    }
    
    output <- list(corr.chase = chase, cong.chase = congruence)
    
    return(output)
  }


hierarchical_paths <-
  function (comp.corr, component = "levelnum")
    #now just enter any component on the level to get whole level's output
  {
    comp.corrcross <- list()
    comp.corrcurr <- data.frame()
    
    for (i in 1:length(comp.corr))
      #running forward through the list
    {
      comp.corrcurr <- as.data.frame(comp.corr[[i]])
      
      if (component %in% colnames(comp.corrcurr))
        #if the component we're interested is in the matrix
      {
        comp.corrcurr[abs(comp.corrcurr) < .3] <- NA #remove small corrs
        comp.corrcurr[abs(comp.corrcurr) >= .9] <-
          NA #remove redundant corrs
        comp.corrcross[[i]] <- comp.corrcurr
      }
      else
      {
        comp.corrcross[[i]] <- NULL
      }
    }
    
    comp.corrcross <-
      comp.corrcross[-which(sapply(comp.corrcross, is.null))]
    
    if (component == "b1" || component == "b2")
    {
      comp.corrcross <- comp.corr[[1]]
    }
    
    return(comp.corrcross)
  }


negative_paths <-
  function (comp.corr, component = "levelnum")
    #now just enter any component on the level to get whole level's output
  {
    comp.corrcross <- list()
    comp.corrcurr <- data.frame()
    
    for (i in 1:length(comp.corr))
      #running forward through the list
    {
      comp.corrcurr <- as.data.frame(comp.corr[[i]])
      
      if (component %in% colnames(comp.corrcurr))
        #if the component we're interested is in the matrix
      {
        comp.corrcurr[comp.corrcurr > -.3] <-
          NA #remove small and positive corrs
        comp.corrcross[[i]] <- comp.corrcurr
      }
      else
      {
        comp.corrcross[[i]] <- NULL
      }
    }
    
    comp.corrcross <-
      comp.corrcross[-which(sapply(comp.corrcross, is.null))]
    
    if (component == "b1" || component == "b2")
    {
      comp.corrcross <- comp.corr[[1]]
    }
    
    return(comp.corrcross)
  }



######STEP 3: INPUT COR MATRIX BASE
##look at number of components to extract
fa.parallel(baselineCor, n.obs = ) #in n.obs = put number of subjects (around 10,000)

#5- level hierchary 
bassBase <-
  extended_bassAckward(baselineCor, num.comp = 5, fm = "pca")

options(max.print = 999999) #increases output lines

#component loadings at each level [[x]] specified level 'x'
bassBaseLoad1 <- bassBase$comp.load[[1]]
bassBaseLoad2 <- bassBase$comp.load[[2]]
bassBaseLoad3 <- bassBase$comp.load[[3]]
bassBaseLoad4 <- bassBase$comp.load[[4]]
bassBaseLoad5 <- bassBase$comp.load[[5]]

bassBase$comp.corr #correlations between all levels
bassBase$cong #congruence coefficients between all levels

redundant_path(bassBase$comp.corr, bassBase$cong, "e5") #enter last component in the hierarhcy (here with 5 levels it's 'e5' etc.)
#This summary output identifies chains of redundant components in the structure

#This lets you look at more detail in the structure (all higher-order correlations >.4)
hierarchical_paths(bassBase$comp.corr, "b1") #This lets you look at more detail in the structure (all higher-order correlations >.4)
hierarchical_paths(bassBase$comp.corr, "c1")
hierarchical_paths(bassBase$comp.corr, "d1")
hierarchical_paths(bassBase$comp.corr, "e1")

#this one plots all negative loadings <-.3
negative_paths(bassBase$comp.corr, "b1") 
negative_paths(bassBase$comp.corr, "c1")
negative_paths(bassBase$comp.corr, "d1")
negative_paths(bassBase$comp.corr, "e1")





######STEP 4: INPUT COR MATRIX ONE YEAR
##look at number of components to extract
fa.parallel(oneyearCor, n.obs = ) #in n.obs = put number of subjects (around 10,000)

#5- level hierchary 
bassOneYear <-
  extended_bassAckward(oneyearCor, num.comp = 5, fm = "pca")

options(max.print = 999999) #increases output lines

#component loadings at each level [[x]] specified level 'x'
bassOneYearLoad1 <- bassOneYear$comp.load[[1]]
bassOneYearLoad2 <- bassOneYear$comp.load[[2]]
bassOneYearLoad3 <- bassOneYear$comp.load[[3]]
bassOneYearLoad4 <- bassOneYear$comp.load[[4]]
bassOneYearLoad5 <- bassOneYear$comp.load[[5]]

bassOneYear$comp.corr #correlations between all levels
bassOneYear$cong #congruence coefficients between all levels

redundant_path(bassOneYear$comp.corr, bassOneYear$cong, "e5") #enter last component in the hierarhcy (here with 5 levels it's 'e5' etc.)
#This summary output identifies chains of redundant components in the structure

#This lets you look at more detail in the structure (all higher-order correlations >.4)
hierarchical_paths(bassOneYear$comp.corr, "b1") #This lets you look at more detail in the structure (all higher-order correlations >.4)
hierarchical_paths(bassOneYear$comp.corr, "c1")
hierarchical_paths(bassOneYear$comp.corr, "d1")
hierarchical_paths(bassOneYear$comp.corr, "e1")

#this one plots all negative loadings <-.3
negative_paths(bassOneYear$comp.corr, "b1") 
negative_paths(bassOneYear$comp.corr, "c1")
negative_paths(bassOneYear$comp.corr, "d1")
negative_paths(bassOneYear$comp.corr, "e1")




######STEP 5: INPUT COR MATRIX TWO YEAR
##look at number of components to extract
fa.parallel(twoyearCor, n.obs = ) #in n.obs = put number of subjects (around 10,000)

#5- level hierchary 
bassTwoYear <-
  extended_bassAckward(twoyearCor, num.comp = 5, fm = "pca")

options(max.print = 999999) #increases output lines

#component loadings at each level [[x]] specified level 'x'
bassTwoYearLoad1 <- bassTwoYear$comp.load[[1]]
bassTwoYearLoad2 <- bassTwoYear$comp.load[[2]]
bassTwoYearLoad3 <- bassTwoYear$comp.load[[3]]
bassTwoYearLoad4 <- bassTwoYear$comp.load[[4]]
bassTwoYearLoad5 <- bassTwoYear$comp.load[[5]]

bassTwoYear$comp.corr #correlations between all levels
bassTwoYear$cong #congruence coefficients between all levels

redundant_path(bassTwoYear$comp.corr, bassTwoYear$cong, "e5") #enter last component in the hierarhcy (here with 5 levels it's 'e5' etc.)
#This summary output identifies chains of redundant components in the structure

#This lets you look at more detail in the structure (all higher-order correlations >.4)
hierarchical_paths(bassTwoYear$comp.corr, "b1") #This lets you look at more detail in the structure (all higher-order correlations >.4)
hierarchical_paths(bassTwoYear$comp.corr, "c1")
hierarchical_paths(bassTwoYear$comp.corr, "d1")
hierarchical_paths(bassTwoYear$comp.corr, "e1")

#this one plots all negative loadings <-.3
negative_paths(bassTwoYear$comp.corr, "b1") 
negative_paths(bassTwoYear$comp.corr, "c1")
negative_paths(bassTwoYear$comp.corr, "d1")
negative_paths(bassTwoYear$comp.corr, "e1")


#######STEP 6: FACTOR CONGRUENCE 
congruenceBaseOne <- factor.congruence(bassBase5,bassOneYear5)
congruenceBaseTwo <- factor.congruence(bassBase5,bassTwoYearLoad5)


