###########
#Called by toxboot(). This function will will take
#data table datchemval with columns logc, resp, bmad
#
#Function returns datchemsample which is a data table with number of columns equal to the number of sampled replicates
#and number of rows equal to the number of measured points. For example, a chemical with 6 doses each measured
#3 times with 1000 boot strap replicates will return at data table with 18 rows and 1000 columns.

if(getRversion() >= "2.15.1")  utils::globalVariables(c("index", "sd", "median"))


#' Generates bootstrap samples of dose response data
#'
#' \code{toxbootReplicates} takes input dose response data and returns
#' a matrix of sampled dose response values.
#'
#' @param datchemval data.table, columns logc, resp, bmad
#' @param boot_method string, specifies method. Default is 'smooth'.
#' @param replicates number of bootstrap samples. Default 100
#'
#' @details
#' Accepted methods are "case", "smooth", "residuals_hill", "wild_hill",
#' "residuals_gnls", "wild_gnls", "watt_normal", "watt_student_df5"
#'
#' @seealso \code{\link{toxboot}}
#'
#' @import data.table
#' @importFrom tcpl tcplFit
#' @importFrom stats mad sd median rnorm rt
#'
#' @export
toxbootReplicates <- function(datchemval,
                              boot_method = 'smooth',
                              replicates = 100
                              ){
  #check to make sure boot_method is supported, return error if not
  methods <- c("case",
               "smooth",
               "residuals_hill",
               "wild_hill",
               "residuals_gnls",
               "wild_gnls",
               "watt_normal",
               "watt_student_df5")
  if(!(boot_method %in% methods)){
    stop("boot_method not supported")
  }

  #datchemstat is used in multiple cases,
  #best to have it before the different methods to make sure it's done the same way
  #get mean, sd, median of values at given conc
  datchemstat = data.table(logc=sort(unique(datchemval$logc)))
  for (conc in unique(datchemstat$logc)){
    datchemstat[logc == conc, sd := sd(datchemval[logc == conc,resp])]
    datchemstat[logc == conc, mean := mean(datchemval[logc == conc,resp])]
    datchemstat[logc == conc, median := median(datchemval[logc == conc,resp])]

    #Adds column that gives the number of repeats for that chemical at that concentration
    datchemstat[logc == conc, number_measurements := length(datchemval[logc == conc,resp])]
  }

  bmad = datchemval$bmad[1]

  #now generate the samples depending on method

  if(boot_method == 'watt_normal'){
    #Make data frame with number of columns of sampled values equal to replicates
    tempmat <- c()
    for (conc in datchemstat[,logc]){   #REMOVE SORT AND UNIQUE?
      number_measurements <- datchemstat[logc == conc, number_measurements]
      resp_vect <- datchemval[logc == conc, resp] #gets the measured responses at this concentration
      if(number_measurements > 1){
        for (numpoint in 1:number_measurements){
          mean_resp <- datchemstat[logc == conc, mean]
          sd_resp <- datchemstat[logc == conc, sd]
          vect <- rnorm(n = replicates,
                        mean=mean_resp,
                        sd = sd_resp)
          tempmat <- rbind(tempmat, vect)
        }
      }
      else{
        vect <- rep(resp_vect, times = replicates) #This is necessary when only one dose, as sample will do 1:X if length of X == 1
        tempmat <- rbind(tempmat, vect)
      }
    }
  } #end method 'watt_normal'

  if(boot_method == 'watt_student_df5'){
    #Make data frame with number of columns of sampled values equal to replicates
    tempmat <- c()
    for (conc in datchemstat[,logc]){   #REMOVE SORT AND UNIQUE?
      number_measurements <- datchemstat[logc == conc, number_measurements]
      resp_vect <- datchemval[logc == conc, resp] #gets the measured responses at this concentration
      if(number_measurements > 1){
        for (numpoint in 1:number_measurements){
          mean_resp <- datchemstat[logc == conc, mean]
          sd_resp <- datchemstat[logc == conc, sd]
          vect <- rt(n = replicates, df = 5)*sd_resp*sqrt((5-2)/5)+mean_resp
          tempmat <- rbind(tempmat, vect)
        }
      }
      else{
        vect <- rep(resp_vect, times = replicates) #This is necessary when only one dose, as sample will do 1:X if length of X == 1
        tempmat <- rbind(tempmat, vect)
      }
    }
  } #end method 'watt_student_df5'

  if(boot_method == 'case'){
    #This method samples each only actual measured values
    #At each concentration, there are N samples where N is equal to
    #the number of measurements


    #Make data frame with number of columns of sampled values equal to replicates
    tempmat <- c()
    for (conc in datchemstat[,logc]){   #REMOVE SORT AND UNIQUE?
      number_measurements <- datchemstat[logc == conc, number_measurements]
      resp_vect <- datchemval[logc == conc, resp] #gets the measured responses at this concentration
      if(number_measurements > 1){
        for (numpoint in 1:number_measurements){
          vect <- sample(resp_vect, replicates, replace=T)
          tempmat <- rbind(tempmat, vect)
        }
      }
      else{
        vect <- rep(resp_vect, times = replicates) #This is necessary when only one dose, as sample will do 1:X if length of X == 1
        tempmat <- rbind(tempmat, vect)
      }
    }
  } #end method 'case'

  if(boot_method == 'smooth'){
    #A few options for this one. Does that small amount of zero centered random noise
    #Get added to each measured point, or is this on top of case sampling?
    #For now, I'll mix with case sampling, performing the same case sampling above
    #But adding a noise value
    #
    #Second, what function to use for noise? Going to use normal distribution centered
    #on measured point and stdev = bmad (bmad already multiplied by 1.4826 within mad() function)
    #also pdf www.anawida.de/teach/SS12/compStat/Boots/smoothboot/smoothboot.pdf
    tempmat <- c()
    for (conc in datchemstat[,logc]){   #REMOVE SORT AND UNIQUE?
      number_measurements <- datchemstat[logc == conc, number_measurements]
      resp_vect <- datchemval[logc == conc, resp] #gets the measured responses at this concentration


      if(number_measurements > 1){
        for (numpoint in 1:number_measurements){
          vect <- sample(resp_vect, replicates, replace=T) +
                  rnorm(n = replicates, mean=0, sd = bmad)
          tempmat <- rbind(tempmat, vect)
        }
      }
      else{
        vect <- resp_vect + #This is necessary when only one dose, as sample will do 1:X if length of X == 1
          rnorm(n = replicates, mean=0, sd = bmad)
        tempmat <- rbind(tempmat, vect)

      }
    }
  } #end method 'smooth'

  if(boot_method == 'residuals_hill'){
    #Get parameters from pipeline fit with actual points
    normalfit <- tcplFit(datchemval$logc, datchemval$resp, datchemval$bmad)#Fit the input data as usual, to compare
    normalfit_hill_ga <- normalfit$hill_ga
    normalfit_hill_tp <- normalfit$hill_tp
    normalfit_hill_gw <- normalfit$hill_gw

    #Get vector of residuals for each measured point
    datchemval[,fitval := hill_curve(hill_tp = normalfit_hill_tp,
                                     hill_ga = normalfit_hill_ga,
                                     hill_gw = normalfit_hill_gw,
                                     lconc = logc)]
    datchemval[,residual := resp - fitval]
    #now, column residual lists the residuals and
    #column fitval are the values from the pipeline fit at measured concentrations

    #Currently issue for those that aren't hits, presumably NAs being put in for fitval or residual
    #for now, set these to 0, but will need to have better way of handling this.
    datchemval[is.na(residual) , residual := 0]

    #
    tempmat <- c()
    for (conc in datchemval[,logc]){   #removed sort, using setkey before calling tcplBootReplicates now so already sorted
      fitval <- hill_curve(hill_tp = normalfit_hill_tp,
                           hill_ga = normalfit_hill_ga,
                           hill_gw = normalfit_hill_gw,
                           lconc = conc)
      #like above note, if fitval is NA, set to 0 to avoid error, but figure out better way of handling this
      if (is.na(fitval)){
        fitval <- 0
      }
      vect <- fitval + sample(datchemval[,residual], replicates, replace=T)
      tempmat <- rbind(tempmat, vect)
    }


  } #end method 'resampling_hill'

  if(boot_method == 'wild_hill'){
    #Multiple options listed for how to sample the residual. Compared to resampling residuals,
    #wild keeps the residual and calculated response paired, and then scales the residual based
    #on some distribution, centered on the fit response. Going to use normal distribution with sd of 1

    #Borrows a lot of code from 'resampling'

    #Get parameters from pipeline fit with actual points
    normalfit <- tcplFit(datchemval$logc, datchemval$resp, datchemval$bmad)#Fit the input data as usual, to compare
    normalfit_hill_ga <- normalfit$hill_ga
    normalfit_hill_tp <- normalfit$hill_tp
    normalfit_hill_gw <- normalfit$hill_gw

    #Get vector of residuals for each measured point
    datchemval[,fitval := hill_curve(hill_tp = normalfit_hill_tp,
                                     hill_ga = normalfit_hill_ga,
                                     hill_gw = normalfit_hill_gw,
                                     lconc = logc)]
    datchemval[,residual := fitval - resp]
    #now, column residual lists the residuals and
    #column fitval are the values from the pipeline fit at measured concentrations

    #Currently issue for those that aren't hits, presumably NAs being put in for fitval or residual
    #for now, set these to 0, but will need to have better way of handling this.
    datchemval[is.na(residual) , residual := 0]

    ###
    #I want to be able to apply the for loop before over each row, and have access to both fitval and the residual
    #since concs are repeated, I can't reference the row by them, such as datchemval[logc==conc,fitval], as this
    #would return more than 1
    #best option for now seems to be to make a column of 1:nrow
    datchemval[,index:=c(1:nrow(datchemval))]
    #
    tempmat <- c()
    for (rowindex in datchemval[,index]){
      fitval <- datchemval[index==rowindex, fitval]
      residual <- datchemval[index==rowindex, residual]

      vect <- fitval + residual*rnorm(n = replicates, mean=0, sd = 1)
      tempmat <- rbind(tempmat, vect)
    }
  } #end method 'wild_hill'

  if(boot_method == 'residuals_gnls'){
    #Get parameters from pipeline fit with actual points
    normalfit <- tcplFit(datchemval$logc, datchemval$resp, datchemval$bmad)#Fit the input data as usual, to compare
    normalfit_gnls_ga <- normalfit$gnls_ga
    normalfit_gnls_tp <- normalfit$gnls_tp
    normalfit_gnls_gw <- normalfit$gnls_gw
    normalfit_gnls_la <- normalfit$gnls_la
    normalfit_gnls_lw <- normalfit$gnls_lw

    #Get vector of residuals for each measured point
    datchemval[,fitval := gnls_curve(top = normalfit_gnls_tp,
                                     ga = normalfit_gnls_ga,
                                     gw = normalfit_gnls_gw,
                                     la = normalfit_gnls_la,
                                     lw = normalfit_gnls_lw,
                                     lconc = logc)]
    datchemval[,residual := resp - fitval]
    #now, column residual lists the residuals and
    #column fitval are the values from the pipeline fit at measured concentrations

    #Currently issue for those that aren't hits, presumably NAs being put in for fitval or residual
    #for now, set these to 0, but will need to have better way of handling this.
    datchemval[is.na(residual) , residual := 0]

    #
    tempmat <- c()
    for (conc in datchemval[,logc]){   #removed sort, using setkey before calling tcplBootReplicates now so already sorted
      fitval <- gnls_curve(top = normalfit_gnls_tp,
                           ga = normalfit_gnls_ga,
                           gw = normalfit_gnls_gw,
                           la = normalfit_gnls_la,
                           lw = normalfit_gnls_lw,
                           lconc = conc)
      #like above note, if fitval is NA, set to 0 to avoid error, but figure out better way of handling this
      if (is.na(fitval)){
        fitval <- 0
      }
      vect <- fitval + sample(datchemval[,residual], replicates, replace=T)
      tempmat <- rbind(tempmat, vect)
    }


  } #end method 'resampling_gnls'

  if(boot_method == 'wild_gnls'){
    #Multiple options listed for how to sample the residual. Compared to resampling residuals,
    #wild keeps the residual and calculated response paired, and then scales the residual based
    #on some distribution, centered on the fit response. Going to use normal distribution with sd of 1

    #Borrows a lot of code from 'resampling'

    #Get parameters from pipeline fit with actual points
    normalfit <- tcplFit(datchemval$logc, datchemval$resp, datchemval$bmad)#Fit the input data as usual, to compare
    normalfit_gnls_ga <- normalfit$gnls_ga
    normalfit_gnls_tp <- normalfit$gnls_tp
    normalfit_gnls_gw <- normalfit$gnls_gw
    normalfit_gnls_la <- normalfit$gnls_la
    normalfit_gnls_lw <- normalfit$gnls_lw

    #Get vector of residuals for each measured point
    datchemval[,fitval := gnls_curve(top = normalfit_gnls_tp,
                                     ga = normalfit_gnls_ga,
                                     gw = normalfit_gnls_gw,
                                     la = normalfit_gnls_la,
                                     lw = normalfit_gnls_lw,
                                     lconc = logc)]
    datchemval[,residual := fitval - resp]
    #now, column residual lists the residuals and
    #column fitval are the values from the pipeline fit at measured concentrations

    #Currently issue for those that aren't hits, presumably NAs being put in for fitval or residual
    #for now, set these to 0, but will need to have better way of handling this.
    datchemval[is.na(residual) , residual := 0]

    ###
    #I want to be able to apply the for loop before over each row, and have access to both fitval and the residual
    #since concs are repeated, I can't reference the row by them, such as datchemval[logc==conc,fitval], as this
    #would return more than 1
    #best option for now seems to be to make a column of 1:nrow
    datchemval[,index:=c(1:nrow(datchemval))]
    #
    tempmat <- c()
    for (rowindex in datchemval[,index]){
      fitval <- datchemval[index==rowindex, fitval]
      residual <- datchemval[index==rowindex, residual]

      vect <- fitval + residual*rnorm(n = replicates, mean=0, sd = 1)
      tempmat <- rbind(tempmat, vect)
    }
  } #end method 'wild_gnls'

  datchemsample <- data.table(tempmat)
  boot_replicates <- list('tempmat' = tempmat, 'datchemsample' = datchemsample)
  return(boot_replicates)

}
