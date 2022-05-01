# The functions in this file belong to the inTrees package. With the following
# details:

# Package: inTrees
# Title: Interpret Tree Ensembles
# Version: 1.2
# Date: 2018-03-10
# Imports: RRF, arules, gbm, xtable, xgboost, data.table, methods
# Author: Houtao Deng, Xin Guan, Vadim Khotilovich
# Maintainer: Houtao Deng <softwaredeng@gmail.com>
# Description: For tree ensembles such as random forests, regularized random
# forests and gradient boosted trees, this package provides functions for:
# extracting, measuring and pruning rules; selecting a compact rule set;
# summarizing rules into a learner; calculating frequent variable interactions;
# formatting rules in latex code.
# BugReports: https://github.com/softwaredeng/inTrees/issues
# License: GPL (>= 3)
# Packaged: 2018-03-12 04:55:28 UTC; houtaodeng
# NeedsCompilation: no
# Repository: CRAN
# Date/Publication: 2018-03-12 05:35:13 UTC

# Since 2022-02-03, the inTrees package has been archived. Upon uploading a new
# version of the inTrees package to the CRAN, the maintainers of the CRE
# package will remove this file and import the inTrees package directly from
# CRAN.

# Loaded libraries for inTrees:
# stats (in common with CRE)
# xtable
# data.table (in common with CRE)
# RRF from RRF
# getTree from RRF
# pretty.gbm.tree from gbm
# xgb.model.dt.tree from xgboost
# arules
# as from methods

# Used functions:
# inTrees_RF2List
# inTrees_GBM2List
# inTrees_extractRules
# inTrees_pruneRule
# inTrees_getRuleMetric


voteAllRules <-
  function(ruleMetric,X,type="r",method="median"){
    xVoteList = vector("list",nrow(X))
    predY <- rep("",nrow(X))
    for(i in 1:nrow(ruleMetric)){
      ixMatch <- eval(parse(text=paste("which(",ruleMetric[i,"condition"], ")"))  )
      if(length(ixMatch)==0) next
      for(ii in ixMatch){
        xVoteList[[ii]] = c(xVoteList[[ii]], ruleMetric[i,"pred"])
      }
    }
    for(i in 1:length(xVoteList)){
      thisV <- xVoteList[[i]]
      if(length(thisV)==0) next
      if(type == "c") predY[i] <- names(table(thisV)[which.max(table(thisV))])
      if(type == "r"){
        thisV = as.numeric(thisV)
        if(method == "median"){
          predY[i] <- median(thisV)
        }else{
          predY[i] <- mean(thisV)
        }
      }

    }
    if(type=="r") predY <- as.numeric(predY)
    return(predY)
  }


treeVisit <-
  function(tree,rowIx,count,ruleSet,rule,levelX,length,max_length,digits=NULL)
  {
    if( tree[rowIx,"status"] == -1 | length == max_length ){
      count = count + 1
      ruleSet[[count]] = rule
      return(list(ruleSet = ruleSet, count=count))
    }
    xIx <- tree[rowIx,"split var"]
    xValue <- tree[rowIx,"split point"]
    if(is.integer(digits)) xValue <- round(tree[rowIx,"split point"], digits)

    if(is.null(levelX[[xIx]])){
      lValue <- paste("X[,",xIx, "]<=",xValue,sep="")
      rValue <- paste("X[,",xIx, "]>",xValue,sep="")
    }else{
      xValue<- which(as.integer(intToBits(as.integer(xValue)))>0)
      lValue <- levelX[[xIx]][xValue]
      rValue <- setdiff(levelX[[xIx]],lValue)
      #   lValue <- paste("X[,",xIx, "]%in% '",lValue,"'",sep="")
      #   rValue <- paste("X[,",xIx, "]%in% '",rValue,"'",sep="")
    }
    xValue <- NULL
    ruleleft <- rule
    if(length(ruleleft)==0)
    {
      ruleleft[[as.character(xIx)]] <- lValue
    }else{
      if(as.character(xIx) %in% ls(ruleleft)) {
        if(!is.null(levelX[[xIx]])){
          lValue <- intersect(ruleleft[[as.character(xIx)]],lValue)
          ruleleft[[as.character(xIx)]] <- lValue
        }else{
          ruleleft[[as.character(xIx)]] <- paste(ruleleft[[as.character(xIx)]], "&", lValue)
        }
      }else{
        ruleleft[[as.character(xIx)]] <- lValue
      }
    }

    #thisItem = paste("X[,",xIx, "] %in% ", nxValue, sep="")
    ruleright <- rule
    if(length(ruleright)==0)
    {
      ruleright[[as.character(xIx)]] <- rValue
    }else{
      if(as.character(xIx) %in% ls(ruleright)) {
        if(!is.null(levelX[[xIx]])){
          rValue <- intersect(ruleright[[as.character(xIx)]],rValue)
          ruleright[[as.character(xIx)]] <- rValue
        }else{
          ruleright[[as.character(xIx)]] <- paste(ruleright[[as.character(xIx)]], "&", rValue)
        }
      }else{
        ruleright[[as.character(xIx)]] <- rValue
      }
    }

    thisList = treeVisit(tree, tree[rowIx,"left daughter"],count,ruleSet,ruleleft,levelX,length+1,max_length,digits)
    ruleSet = thisList$ruleSet; count = thisList$count

    thisList = treeVisit(tree, tree[rowIx,"right daughter"],count,ruleSet,ruleright,levelX,length+1,max_length,digits)
    ruleSet = thisList$ruleSet; count = thisList$count

    return(list(ruleSet = ruleSet, count=count))
  }


sortRule <-
  function(M,decreasing=TRUE){
    qIx = order((1- as.numeric(M[,"err"])),
                as.numeric(M[,"freq"]),
                -as.numeric(M[,"len"]),
                decreasing=decreasing)
    return(M[qIx,])
  }

singleRuleList2Exec <-
  function(ruleList,typeX){ #numeric: 1; categorical: 2s
    #ruleExec <- "which("
    ruleExec <- ""
    vars <- ls(ruleList)
    #ruleL <- length(unique(vars))
    vars <- vars[order(as.numeric(vars))]
    for(i in 1:length(vars)){
      if(typeX[as.numeric(vars[i])]==2){
        values <- paste("c(",paste(  paste("'",ruleList[[vars[i]]],"'",sep="")    ,collapse=","),")",sep="")
        tmp = paste("X[,",vars[i], "] %in% ", values, sep="")
      }else{
        tmp = ruleList[[vars[i]]]
      }
      if(i==1)ruleExec <- paste(ruleExec, tmp,sep="")
      if(i>1)ruleExec <- paste(ruleExec, " & ", tmp, sep="")
    }
    #ruleExec <- paste(ruleExec,")",sep="")
    return(c(ruleExec))
  }

selectRuleRRF <-
  function(ruleMetric,X,target){
    ruleI = sapply(ruleMetric[,"condition"],rule2Table,X,target)
    coefReg <- 0.95 - 0.01*as.numeric(ruleMetric[,"len"])/max(as.numeric(ruleMetric[,"len"]))
    rf <- RRF(ruleI,as.factor(target), flagReg = 1, coefReg=coefReg, mtry = (ncol(ruleI)*1/2) , ntree=50, maxnodes= 10,replace=FALSE)
    imp <- rf$importance/max(rf$importance)
    feaSet <- which(imp > 0.01)
    ruleSetPrunedRRF <- cbind(ruleMetric[feaSet,,drop=FALSE],impRRF=imp[feaSet])
    ix = order(as.numeric(ruleSetPrunedRRF[,"impRRF"]),
               - as.numeric(ruleSetPrunedRRF[,"err"]),
               - as.numeric(ruleSetPrunedRRF[,"len"]),
               decreasing=TRUE)
    ruleSelect <- ruleSetPrunedRRF[ix,,drop=FALSE]
    return(ruleSelect)
  }


ruleList2Exec <-
  function(X,allRulesList){
    typeX = getTypeX(X)
    ruleExec <- unique(t(sapply(allRulesList,singleRuleList2Exec,typeX=typeX)))
    ruleExec <- t(ruleExec)
    colnames(ruleExec) <- "condition"
    return(ruleExec)
  }


rule2Table <-
  function(ruleExec,X,target){
    I <- rep(0,nrow(X))
    ruleExec <- paste("which(", ruleExec, ")")
    ixMatch <- eval(parse(text=ruleExec))
    if(length(ixMatch)>0) I[ixMatch] <- 1
    names(I) = NULL
    return(I)
  }


pruneSingleRule <-
  function(rule, X, target, maxDecay, typeDecay){
    # typeDecay = 1: relative error increase; otherwise: absolute error increase

    #A <- gregexpr("X\\[,[0-9]+\\]", s)
    newRuleMetric <- measureRule(rule["condition"],X,target)
    errOrig <- as.numeric(newRuleMetric["err"])
    ruleV <- unlist(strsplit(rule["condition"],split= " & "))
    pred <- rule["pred"]
    # newRule <- NULL
    if(length(ruleV)==1) return(newRuleMetric)
    for(i in length(ruleV):1){
      restRule <- ruleV[-i]
      restRule <- paste(restRule,collapse= " & ")
      metricTmp <- measureRule(restRule,X,target,pred)
      errNew <- as.numeric(metricTmp["err"])
      if(typeDecay == 1){
        decay <- (errNew-errOrig)/max(errOrig,0.000001)
      }else{
        decay <- (errNew-errOrig)
      }
      if( decay <= maxDecay){
        #if( errNew-errOrig <= maxDecay){
        ruleV <- ruleV[-i]
        # newRule saves the last changed rule and metrics
        newRuleMetric <- metricTmp
        if(length(ruleV)<=1)break
      }
    }
    return(newRuleMetric)
    #rule["condition"] <- paste(ruleV,collapse= " & ")
    #return(rule)
  }


inTrees_pruneRule <-
  function(rules,X,target, maxDecay = 0.05, typeDecay = 2){
    newRuleMetric <- NULL
    for(i in 1:nrow(rules)){
      newRuleMetric <- rbind(newRuleMetric, pruneSingleRule(rules[i,],X,target, maxDecay, typeDecay))
    }
    return(newRuleMetric)
  }


presentRules <-
  function(rules,colN,digits=NULL){
    for(i in 1:nrow(rules[,"condition",drop=FALSE])){
      if(is.numeric(digits)){
        digits <- as.integer(abs(digits))
        rules[,"freq"] <- round(as.numeric( rules[,"freq"]),digits=digits)
        rules[,"err"] <- round(as.numeric( rules[,"err"]),digits=digits)
      }
      A <- regexpr("X\\[,1\\]==X\\[,1\\]", rules[i,"condition"])
      thisPos <- as.numeric(A[[1]])
      thisLen <- attr(A, "match.length")
      if(thisPos > 0){
        origStr <- substr(rules[i,"condition"], thisPos, thisPos+thisLen-1)
        rules[i,"condition"] <- gsub(origStr, "Else", rules[i,"condition"], fixed=TRUE)
      }
      while(TRUE){
        A <- regexpr("X\\[,[0-9]+\\]", rules[i,"condition"])
        thisPos <- as.numeric(A[[1]])
        thisLen <- attr(A, "match.length")
        if(thisPos <= 0) break
        origStr <- substr(rules[i,"condition"], thisPos, thisPos+thisLen-1)
        ix <- as.numeric(gsub("\\D", "", origStr))
        colStr <- colN[ix]
        rules[i,"condition"] <- gsub(origStr, colStr, rules[i,"condition"], fixed=TRUE)
      }
    }
    return(rules)
  }


measureRule <-
  function(ruleExec,X,target,pred=NULL,regMethod="mean"){
    len <- length(unlist(strsplit(ruleExec, split=" & ")))
    origRule <- ruleExec
    ruleExec <- paste("which(", ruleExec, ")")
    ixMatch <- eval(parse(text=ruleExec))
    if(length(ixMatch)==0){
      v <- c("-1","-1", "-1", "", "")
      names(v) <- c("len","freq","err","condition","pred")
      return(v)
    }
    ys <- target[ixMatch]
    freq <- round(length(ys)/nrow(X),digits=3)

    if(is.numeric(target))
    {
      if(regMethod == "median"){
        ysMost = median(ys)
      }else{
        ysMost <- mean(ys)
      }
      err <- sum((ysMost - ys)^2)/length(ys)
    }else{
      if(length(pred)>0){ #if pred of the rule is provided
        ysMost = as.character(pred)
      }else{
        ysMost <- names(which.max(  table(ys))) # get back the first max
      }
      ly <- sum(as.character(ys)==ysMost)
      conf <- round(ly/length(ys),digits=3)
      err <- 1 - conf
    }
    rule <- origRule

    v <- c(len, freq, err, rule, ysMost)
    names(v) <- c("len","freq","err","condition","pred")
    return(v)
  }


lookupRule <-
  function(rules,strList){
    ix <- grep(strList[1], rules[,"condition"],fixed = TRUE)
    if(length(strList)>=2){
      for(i in 2:length(strList)){
        ix2 <- grep(strList[i], rules[,"condition"],fixed = TRUE)
        ix <- intersect(ix,ix2)
      }
    }
    if(length(ix)>=1)return(rules[ix,,drop=FALSE])
    if(length(ix)==0)return(NULL)
  }


.Random.seed <-
  c(403L, 1L, -788367838L, 165038100L, 79326359L, 1084589709L,
    -858183772L, -591177334L, 1733125621L, 494884943L, -1515335850L,
    -699963904L, 1538717507L, 35303137L, -1791529904L, 255227054L,
    -453349287L, 38236267L, -1868184102L, 33222300L, -1151401825L,
    -327804171L, 278390668L, -1144461726L, 643938877L, 1113875639L,
    -582247586L, -1235943784L, 1679588859L, 1326145433L, 574412840L,
    -1766784810L, 2070021457L, 451572739L, 387595154L, -868542172L,
    44277735L, 700580925L, 1457607348L, -155623142L, -242937243L,
    2035889919L, -1457280122L, -807080688L, -1608464973L, -194906543L,
    -1458522624L, 1097044702L, -1041399511L, -960647973L, -1387045910L,
    825961356L, -554603313L, -880520059L, 186998588L, -677143022L,
    1873050893L, -1270299065L, 286260270L, -196648568L, 1367357579L,
    1495405161L, -688386504L, 1429341926L, -103357823L, -1947120109L,
    -186582590L, -2028981644L, 458658935L, -707854995L, -1603624444L,
    1392165162L, 42792597L, -2042307281L, 1109682230L, 1385643104L,
    -1563856221L, 1110336961L, -1124568656L, 558379214L, 77475385L,
    -1618816949L, 510403066L, 278764796L, -2128350913L, 31378965L,
    -640496340L, -1920762366L, 1080222941L, 1021755351L, -1657595202L,
    648682744L, 352977627L, 386627897L, -980271992L, -2074342346L,
    828758129L, -517100893L, 888459250L, 157852484L, 1212623879L,
    -975074851L, -167037228L, -1656792582L, 1538389317L, -276225633L,
    -1943829722L, 1116773296L, 412747859L, 2099938161L, 1769575072L,
    1976731198L, 844148041L, 1659450363L, 1346631626L, -1200133844L,
    2132337967L, 235924453L, -1354272100L, 1291777778L, -1079138451L,
    1951068327L, 1650840270L, 905650216L, 1761745963L, -358551991L,
    1970669400L, 1600459270L, -113640479L, -2102503053L, 284367330L,
    -1572233900L, -802958249L, -923232179L, 265100260L, -1809327670L,
    727523381L, -1069597041L, -1329524842L, -2005544128L, -1243832829L,
    352076833L, 2015176464L, -846701330L, 1772668057L, -43462357L,
    798926874L, -1609391396L, 892601055L, -1406419787L, -719693236L,
    -428513630L, -1769435651L, -1339955465L, 1555509278L, 1750442712L,
    -1708652997L, -533597223L, -225427096L, -1849206634L, 1917654545L,
    -814747709L, -1602587694L, -2129645980L, 480681383L, 473043709L,
    -989206796L, 132985306L, 1303713061L, 1410200639L, -2132819002L,
    -729362352L, 420813939L, 802615441L, 1284640960L, 2041944734L,
    -1215662487L, 1369540251L, 1319283626L, -1656926260L, -1470987633L,
    449211077L, -1854946692L, 1472990930L, -630502835L, 368903943L,
    -117454098L, 935698376L, -63119157L, 239010089L, 1207818616L,
    -1359122778L, -1103111999L, -1240927917L, -1207460350L, 1302939444L,
    887825591L, -1305488211L, -2113392188L, -598797846L, 607030869L,
    1233190895L, 513241078L, 1897474336L, -1970157853L, -1276256383L,
    -1138365968L, -718546802L, -1954488327L, 317420939L, 428331450L,
    -1658466116L, 1871459327L, 1621442901L, -1144702356L, -1739927870L,
    -1476331235L, 528609175L, -1120523522L, 462770360L, -2109556069L,
    1988679929L, -581219768L, -396421770L, 3024049L, -1347434269L,
    262293426L, 2046015492L, 2110737223L, 1762933488L, 1568427588L,
    -377765288L, 419557178L, 510420464L, -499870020L, -1352042220L,
    750568130L, 531469408L, -1202838660L, -640567344L, 439968034L,
    969054888L, 1371462540L, 1854421180L, 302599986L, 1318341184L,
    833182900L, 1198391880L, -2093980230L, -1499028976L, -1008094484L,
    -941563564L, 1900339474L, 1496993184L, -1663892788L, -836919712L,
    637401026L, -312998104L, -254644308L, -609631428L, 1070983538L,
    -1569563856L, -928920284L, -82515144L, 323387098L, -556611696L,
    -973091076L, 1055009684L, 1395621954L, 255849728L, -841874372L,
    30638416L, -1794649950L, 631406920L, 1740330764L, 999367388L,
    -626348718L, 1599259776L, 1816641620L, 656609256L, -193754886L,
    -1107236016L, -94437588L, -440809100L, -1442878574L, -977778592L,
    -846926452L, -213918880L, 1830410722L, 657699752L, 203175116L,
    342499324L, -503011918L, 795710960L, -381203516L, -994818728L,
    -2024482438L, 607716272L, 566241724L, -149579692L, -1318045758L,
    -1961365472L, 2074892476L, 374026832L, 881816546L, 649150568L,
    289473612L, -31639300L, -337703950L, -932263680L, 2038063476L,
    201763848L, 1780096954L, 1546301904L, -19416084L, -1212694124L,
    1635340754L, 35090784L, 1923215180L, -794494688L, 2031419266L,
    323743464L, -348962580L, -947063748L, -937559182L, -329884048L,
    321082404L, -259298504L, -2033128294L, 452604624L, -2126569284L,
    -378081644L, -1551690110L, 893167808L, 59908924L, -360031088L,
    394927650L, -1751183352L, 1897364748L, -265966436L, 1950875154L,
    -787218560L, -1049129452L, -1094800856L, 1760998714L, 1211009488L,
    1018000492L, 1438288180L, 1571796626L, -869037216L, 1739985740L,
    -1227319328L, -187877470L, -1483880536L, 1102556556L, 37103804L,
    -1043035790L, -602786576L, -1532112828L, 1625350488L, -1687247302L,
    -1741243536L, -1260079044L, -130973932L, -1556240958L, 797965024L,
    -720554116L, 225836752L, 583858338L, -1173812184L, -2009905908L,
    -1046067268L, -67303118L, -863614528L, -1004467660L, -370754616L,
    -1479919430L, 927492496L, -984550932L, 714547796L, -1884139374L,
    -1706119392L, 723970508L, 1779930592L, -792512830L, 538821672L,
    -1564224852L, -190577476L, -804353550L, 1216816L, -375630684L,
    -791402824L, 1831255386L, 1445098896L, 222466940L, -936376044L,
    1747444546L, 833903360L, -1676724292L, -12837040L, 1472955170L,
    -139322040L, 270600716L, 1231284316L, 1547664594L, -488162816L,
    -405729964L, 1081162216L, 994298L, 618594512L, 138087724L, 727966452L,
    1951021458L, 763941984L, 1538237708L, 367350496L, 2034583906L,
    -1273505112L, -1555185076L, 991637884L, -1619466958L, 354750576L,
    605669188L, -189850152L, -1663603590L, 744844848L, -1531041092L,
    -1448878252L, 1410094146L, -1114268256L, 859368124L, 1007548880L,
    -2032198302L, 356765672L, -1445623860L, 860016380L, 700269042L,
    -643544960L, -541891340L, -1734703480L, 1499060026L, 2073727824L,
    -2001140116L, 1277083796L, 1188704594L, 1939406560L, 327897804L,
    1406259360L, 525908866L, 808719080L, -824429844L, -1098572740L,
    -1147334030L, -1966500752L, 1397745188L, -1015479240L, 1872388343L,
    -790393856L, -1982861730L, -1031745237L, 1297756717L, 132887194L,
    -1354428840L, -381546047L, 126959219L, 101497748L, -76205534L,
    -582001353L, 1552227329L, -898629370L, 1477887300L, -2060515211L,
    995346911L, -558656200L, 824829718L, 1504618275L, -752143131L,
    1893748994L, -1063632016L, -236979431L, 1023105803L, 1705732092L,
    -2059770838L, -839489441L, -177356823L, 2147311870L, -1716137748L,
    -678415299L, -179058681L, 1547359216L, -1238327410L, -560300613L,
    -236856419L, 1552965962L, 276048232L, -446423215L, 2100878403L,
    1120781636L, 316704242L, 2008518599L, 56390033L, -477559530L,
    870333812L, 595328005L, 522505295L, -796270520L, 1094473190L,
    1438706451L, -178746251L, 1501181522L, -1261055840L, 977512841L,
    -358602309L, 1104487948L, -1401618534L, -1323948273L, 1396120921L,
    -964175890L, 1750187644L, 1602209517L, 1251209623L, 1283021408L,
    -606449602L, 167721227L, 1840774029L, -627279686L, -664797448L,
    604927137L, -1303793581L, -1734960268L, 186575106L, -1733162601L,
    79246177L, -1444075610L, -513531484L, 21854293L, -96801537L,
    -1724010088L, 1084263094L, -993000637L, 1776483973L, 1797051170L,
    -1251366896L, -125054023L, 766850155L, -858486948L, -868458870L,
    -1367889281L, 1063277513L, 653906014L, -603196660L, 2092661469L,
    -1306094617L, -495474672L, -29989586L, 1793669595L, 447733437L,
    369839146L, -213693240L, 1644562929L, 1043416931L, -1536311452L,
    -1892460910L, -365710489L, -361816783L, -16730506L, -1459140716L,
    -1130179675L, -658627409L, 2012731752L, -759766714L, -937887501L,
    499631701L, -775509454L, 990320640L, -607654679L, -82228389L,
    -1484304980L, -463202886L, 942240623L, -323279623L, -1083229810L,
    1516006492L, 799325901L, -1915291081L, 2051395776L, 148750238L,
    937223275L, -1348123667L, 1347880538L, -982333160L, 1337357953L,
    1181600819L, 1908677204L, 712863330L, -98393865L, -1840729279L,
    66873798L, -1988308220L, 1740363445L, -1505140321L, 677813752L,
    -174623786L, 1755729507L, 630132261L, 234847682L, 2002956976L,
    274627289L, -1590461365L, -1463295300L, 225779946L, 193854751L,
    28304809L, 1393689406L, -1779081684L, 591981693L, 2002056903L,
    235614128L, -1613815346L, -1957299461L, 1333225821L, -1407100278L,
    -732911448L, -226796527L, 1326664963L, -1265759059L)


getTypeX <-
  function(X){
    typeX = rep(0,ncol(X))
    for(i in 1:ncol(X)){ #numeric: 1; categorical: 2s
      if(is.numeric(X[,i])){ typeX[i] = 1 }else{
        typeX[i] = 2
      }
    }
    return(typeX)
  }

inTrees_getRuleMetric <-
  function(ruleExec, X, target){
    #typeX = getTypeX(X)
    #ruleExec <- unique(t(sapply(allRulesList,RuleList2Exec,typeX=typeX)))
    #colnames(ruleExec) <- c("len","condition")
    ruleMetric <- t(sapply(ruleExec[,"condition",drop=FALSE],measureRule,X,target))
    rownames(ruleMetric) = NULL;
    # ruleMetric <- cbind( ruleExec[,1] ,  ruleMetric )
    colnames(ruleMetric) <- c("len","freq","err","condition","pred")
    dIx <- which(ruleMetric[,"len"]=="-1")
    if(length(dIx)>0){
      ruleMetric <- ruleMetric[-dIx,]
      print(paste( length(dIx)," paths are ignored.",sep=""))
    }
    return(ruleMetric)
    #qIx = order((1- as.numeric(ruleMetric[,"err"])),
    #            as.numeric(ruleMetric[,"freq"]),
    #            -as.numeric(ruleMetric[,"len"]),
    #            decreasing=TRUE)
    #return(ruleMetric[qIx,])
  }

# getFreqPattern <-
#   function(ruleMetric,minsup=0.01,minconf=0.5,minlen=1,maxlen=4){
#     # set up
#     predY <- as.character(ruleMetric[,"pred"])
#     rulesV <- strsplit(ruleMetric[,"condition"], split=" & ")
#     for(i in 1:length(rulesV)){
#       rulesV[[i]] = c(rulesV[[i]],paste("=>",predY[i],sep=""))
#     }
#     yrhs= unique(paste("=>",ruleMetric[,"pred"],sep=""))
#     trans1 <- as(rulesV, "transactions")
#     rules1 <- apriori(
#       trans1,
#       parameter = list(supp=minsup,conf=minconf,minlen=minlen,maxlen=maxlen),
#       control = list(verbose=FALSE),
#       appearance = list(none=NULL,rhs =yrhs,default="lhs")
#     )
#     #rules1= sort(rules1, decreasing = FALSE, by = "confidence")
#     #quality = quality(rules1)
#     #qIx = order(quality[,"confidence"],quality[,"support"],decreasing=TRUE)
#     #rules1=rules1[qIx]
#     #quality = quality[qIx,1:2]
#     #inspect(rules1)
#
#     lhs = methods::as(lhs(rules1),"list")
#     rhs = methods::as(rhs(rules1),"list")
#     rhs <- gsub("=>", "", rhs)
#     quality <- quality(rules1)
#     ix_empty <- NULL
#     freqPattern <- NULL
#     for(i in 1:length(lhs)){
#       length_v <- length(lhs[[i]])
#       lhs[[i]] <- paste(lhs[[i]],collapse= " & ")
#       if(nchar(lhs[[i]])==0){
#         ix_empty <- c(ix_empty,i)
#       }
#       freqPattern <- rbind(freqPattern, c(len=length_v, condition=lhs[[i]], pred=rhs[i],
#                                           sup=quality[i,"support"],
#                                           conf=quality[i,"confidence"]) )
#     }
#     if(length(ix_empty)>0)freqPattern <- freqPattern[-ix_empty,]
#     qIx = order(as.numeric(freqPattern[,"sup"]), as.numeric(freqPattern[,"conf"]),
#                 -as.numeric(freqPattern[,"len"]),
#                 decreasing=TRUE)
#     freqPattern <- freqPattern[qIx,c("len","sup","conf","condition","pred")]
#     freqPattern[,c("sup","conf")] <- as.character(round(as.numeric(freqPattern[,c("sup","conf")]),digits=3))
#     return(freqPattern)
#   }


formatGBM <-
  function(gbmList,splitBin,X){
    for(j in 1:length(gbmList$list)){
      a <- gbmList$list[[j]]
      rownames(a) <- 1:nrow(a)
      a$status <- a$SplitVar
      a <- a[,c("LeftNode","RightNode","MissingNode","SplitVar","SplitCodePred","status")]
      a[which(a[,"SplitVar"]>=0),c("SplitVar","LeftNode","RightNode","MissingNode")] <- a[which(a[,"SplitVar"]>=0),c("SplitVar","LeftNode","RightNode","MissingNode")] + 1
      ix <- a$MissingNode[which(a$MissingNode>0)]
      if(length(ix)>0)  a$status[ix] <- 10 #missing #a <- a[-ix,]
      a <- a[,c("LeftNode","RightNode","SplitVar","SplitCodePred","status")]
      cat <- which(sapply(X, is.factor) & !sapply(X, is.ordered))
      ix <- which(a[,"SplitVar"] %in% cat)

      for(i in ix) a[i,"SplitCodePred"] <- splitBin[ a[i,"SplitCodePred"]+1 ]
      colnames(a) <- c("left daughter","right daughter","split var","split point","status")
      gbmList$list[[j]] <- a
    }
    return(gbmList)
  }

inTrees_extractRules <-
  function(treeList,X,ntree=100,maxdepth=6,random=FALSE,digits=NULL){
    if(is.numeric(digits)) digits <- as.integer(abs(digits))

    levelX = list()
    for(iX in 1:ncol(X))
      levelX <- c(levelX,list(levels(X[,iX])))
    # X <- NULL; target <- NULL
    ntree=min(treeList$ntree,ntree)
    allRulesList = list()
    for(iTree in 1:ntree){
      if(random==TRUE){max_length = sample(1:maxdepth,1,replace=FALSE)}else{
        max_length = maxdepth}
      rule = list(); count = 0; rowIx = 1;
      # tree = getTree(rf,iTree,labelVar=FALSE)
      tree <- treeList$list[[iTree]]
      if(nrow(tree)<=1) next # skip if there is no split
      ruleSet = vector("list", length(which(tree[,"status"]==-1)))
      res = treeVisit(tree,rowIx = rowIx,count,ruleSet,rule,levelX,length=0,max_length=max_length,digits=digits)
      allRulesList = c(allRulesList, res$ruleSet)
    }
    allRulesList <- allRulesList[!unlist(lapply(allRulesList, is.null))]
    cat(paste(length(allRulesList)," rules (length<=",
              max_length, ") were extracted from the first ", ntree," trees.","\n",sep=""))

    rulesExec <- ruleList2Exec(X,allRulesList)
    return(rulesExec)
  }


dicretizeVector <-
  function(v,K=3){
    splitV <- quantile(v, probs = seq(0, 1, 1/K), na.rm = FALSE,
                       names = TRUE, type = 3)
    splitV <- splitV[-c(1,length(splitV))]

    numSplit <- length(splitV)  # split points + 1
    if(numSplit==0) return(v)
    newV <- vector("character", length(v))
    newV[which(v<=splitV[1])] = paste("L1",sep="")
    if(numSplit>=2){
      for(jj in 2:numSplit){
        newV[which(  v> splitV[jj-1] & v<=splitV[jj]) ] = paste("L",jj,sep="")
      }
    }
    newV[which( v> splitV[numSplit] ) ] =  paste("L",(numSplit+1),sep="")
    return(newV)
  }


dataSimulate <-
  function(flag=1,nCol=20,nRow=1000){

    if(nCol<=2) stop("nCol must be >= 2.")
    #only the first and the second features are needed
    X <- matrix(runif(nRow*nCol, min=-2, max=2), ncol=nCol)
    target <- rep(-1,nRow)

    #linear case
    if(flag == 3) {
      target <- (X[,1]) + (X[,2])
      ix <- which(target>quantile(target, 1/2));
      target <- target*0-1;
      target[ix] <- 1
    }

    #nonlinear case
    if(flag == 2){
      target <- (X[,1])^2 + 1*(X[,2])^2
      ix <- which(target>quantile(target, 6/10));
      ix <- c(ix,which(target<quantile(target, 1/10)));
      target <- target*0-1;
      target[ix] <- 1
    }

    # team optimization
    if(flag == 1){
      X <- matrix(0,nRow,nCol)
      for(ii in 1:nrow(X)){
        ix <- sample(1:nCol,nCol/2,replace=FALSE)
        X[ii,ix] <- 1
      }
      target <- (xor(X[,1],X[,2]))
      repStr <- function(v){v[v=="1"] <- "Y";v[v=="0"] <- "N";return(v)}
      X <- data.frame(apply(X,2,repStr))
      target[target == FALSE] <- "lose"
      target[target == TRUE] <- "win"
      target <- as.factor(target)

      # X <- data.frame(X)
      # for(jj in 1:ncol(X)){
      #  X[,jj] <- as.factor(X[,jj])
      # }
    }
    return(list(X=X,target=target))
  }


computeRuleInfor <-
  function(instIx,pred,target){
    trueCls <- as.character(target[instIx])
    err <- 1- length(which(trueCls == pred))/length(trueCls)
    return(c(err=err,freq=length(instIx)/length(target)))
  }


buildLearner <-
  function(ruleMetric,X,target,minFreq=0.01){ #Recursive
    ruleMetric <- ruleMetric[,c("len","freq","err","condition","pred"),drop=FALSE]
    learner <- NULL
    listIxInst <- vector("list", nrow(ruleMetric))
    for(i in 1:nrow(ruleMetric)){
      ixMatch <- eval(parse(text=paste("which(",ruleMetric[i,"condition"], ")"))  )
      if(length(ixMatch)==0)next
      listIxInst[[i]] = ixMatch
    }
    ixInstLeft <- 1:length(target)
    while(TRUE){
      infor = NULL
      restErr <- 1 - max(table(target[ixInstLeft]))/length(target[ixInstLeft])
      for(i in 1:length(listIxInst)){
        thisInfor <- computeRuleInfor(listIxInst[[i]], ruleMetric[i,"pred"],target)
        infor <- rbind(infor,c(thisInfor,len=as.numeric(ruleMetric[i,"len"])))
      }
      topIx <- order(infor[,"err"],-infor[,"freq"],infor[,"len"],decreasing=FALSE)
      minSupIx <- which(infor[,"freq"] < minFreq)
      if(length(minSupIx)>0)topIx <- setdiff(topIx,minSupIx)
      if(length(topIx)>0) topIx <- topIx[1]
      if(length(topIx)==0){
        restCondition <- paste("X[,1]==X[,1]")
        restPred <- names(table(target[ixInstLeft]))[which.max(table(target[ixInstLeft]))]
        restSup <- length(ixInstLeft)/length(target)
        thisRuleMetric <- c(len=1,freq=restSup,err=restErr,condition=restCondition,pred=restPred)
        learner <- rbind(learner,thisRuleMetric)
        break
      }else if( infor[topIx,"err"] >= restErr ){
        restCondition <- paste("X[,1]==X[,1]")
        restPred <- names(table(target[ixInstLeft]))[which.max(table(target[ixInstLeft]))]
        restSup <- length(ixInstLeft)/length(target)
        thisRuleMetric <- c(len=1,freq=restSup,err=restErr,condition=restCondition,pred=restPred)
        learner <- rbind(learner,thisRuleMetric)
        break
      }
      #ruleActiveList <- c(ruleActiveList,topIx)
      thisRuleMetric <- ruleMetric[topIx,,drop=FALSE]
      thisRuleMetric[,c("freq","err","len")] <- infor[topIx,c("freq","err","len")]
      learner <- rbind(learner,thisRuleMetric)
      ixInstLeft <- setdiff(ixInstLeft,listIxInst[[topIx]])
      listIxInst <- sapply(listIxInst,setdiff,listIxInst[[topIx]])

      if(length(ixInstLeft)==0) { # if every is targetified perfectly, still set a main target
        restCondition <- paste("X[,1]==X[,1]")
        restPred <- names(table(target))[which.max(table(target))]
        restSup <- 0
        restErr <- 0
        thisRuleMetric <- c(len=1,freq=restSup,err=restErr,condition=restCondition,pred=restPred)
        learner <- rbind(learner,thisRuleMetric)
        break
      }
    }
    rownames(learner) <- NULL
    return(learner)
  }


applyLearner <-
  function(learner,X){
    leftIx <- 1:nrow(X)
    predY <- rep("",nrow(X))
    for(i in 1:nrow(learner)){
      ixMatch <- eval(parse(text=paste("which(",learner[i,"condition"], ")"))  )
      ixMatch <- intersect(leftIx,ixMatch)
      if(length(ixMatch)>0){
        predY[ixMatch] <- learner[i,"pred"]
        leftIx <- setdiff(leftIx,ixMatch)
      }
      if(length(leftIx)==0){
        break
      }
    }
    return(predY)
  }


XGB2List<-
  function(xgb, X)
  {
    feature_names <- colnames(X)
    xt <- xgb.model.dt.tree(feature_names = as.character(1:length(feature_names)), model=xgb)
    # avoid cran note: no visible binding for global variable
    Feature=Split=Yes=No=MissingNode=Missing=Weight=Cover=Prediction=Quality=Node=NULL
    xt[Feature == 'Leaf', Feature := '-1']
    xt[, 'split var' := as.integer(Feature)]
    xt[, 'split point' := Split]
    xt[, 'left daughter' := as.integer(tstrsplit(Yes, '-')[[2]]) + 1]
    xt[, 'right daughter' := as.integer(tstrsplit(No, '-')[[2]]) + 1]
    xt[, MissingNode := as.integer(tstrsplit(Missing, '-')[[2]]) + 1]
    xt[, Weight := Cover]
    xt[, Prediction := Quality]
    xt[, Node := Node + 1]
    xt[, c('ID', 'Yes', 'No', 'Split','Missing', 'Quality', 'Cover', 'Feature') := NULL]
    for (f in c('left daughter', 'right daughter', 'MissingNode'))
      set(xt, which(is.na(xt[[f]])), f, -1)
    treeList <- NULL
    treeList$ntree <- length(unique(xt$Tree))
    treeList$list <- split(xt, by="Tree")
    formatXGB <-
      function(tree){
        rownames(tree) <- 1:nrow(tree)
        tree$status <- ifelse(tree$`split var`==-1,-1,1)
        tree$`split point` <- as.numeric(tree$`split point`)
        tree <- tree[,c("left daughter","right daughter","MissingNode","split var","split point","status")]
        # ix <- tree$MissingNode[which(tree$MissingNode>0)]
        # if(length(ix)>0)  tree$status[ix] <- 10 #missing
        tree <- tree[,c("left daughter","right daughter","split var","split point","status")]
        tree <- as.data.frame(tree)
      }
    treeList$list <- lapply(treeList$list,formatXGB)
    return(treeList)
  }

inTrees_RF2List <-
  function(rf){
    treeList <- NULL
    treeList$ntree <- rf$ntree
    treeList$list <- vector("list",rf$ntree)
    for(i in 1:treeList$ntree){
      treeList$list[[i]] <- getTree(rf,i,labelVar=FALSE)
    }
    return(treeList)
  }

Num2Level <-
  function(rfList,splitV){
    for(i in 1:rfList$ntree){
      rfList$list[[i]] <- data.frame(rfList$list[[i]])
      rfList$list[[i]][,"prediction"] <- data.frame(dicretizeVector(rfList$list[[i]][,"prediction"],splitV))
      colnames(rfList$list[[i]]) <- c("left daughter","right daughter","split var","split point","status","prediction")
    }
    return(rfList)
  }

inTrees_GBM2List <-
  function(gbm1,X){
    treeList <- NULL
    treeList$ntree <- gbm1$n.trees
    treeList$list <- vector("list",gbm1$n.trees)
    for(i in 1:treeList$ntree){
      treeList$list[[i]] <- pretty.gbm.tree(gbm1,i.tree = i)
    }

    v2int <- function(v){sum( (-v+1)/2 * 2^seq(0,(length(v)-1),1)  )}
    #as.integer(intToBits(3616)) pretty.gbm.tree(gbm1,i.tree = 1)
    splitBin = sapply(gbm1$c.splits,v2int)

    return(formatGBM(treeList,splitBin,X))
  }
