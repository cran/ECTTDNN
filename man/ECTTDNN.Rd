\name{ECTTDNN}
\alias{ECTTDNN}
\title{Cointegration Based Time Delay Neural Network Model}
\usage{ECTTDNN(data,type,t,lag_ann,hidden_nodes_ann,r)}
\description{This cointegration based Time Delay Neural Network Model hybrid model allows the researcher to make use of the information extracted by the cointegrating vector as an input in the neural network model.}
%-
  \arguments{
    \item{data}{
      A Multivariate data
    }
    \item{type}{
      Type of cointegration test to be used. Either "trace" or "eigen" value based
    }
    \item{t}{
      Partition value for spliting the data set into training and testing
    }
    \item{lag_ann}{
      Number of lags used for the Time Delay Neural Network Model fitting
    }
    \item{hidden_nodes_ann}{
    Number of hidden nodes used in the Time Delay Neural Network Model
    }
   \item{r}{
    Number of repeats for the Time Delay Neural Network Model
    }
  }
\details{
ECTTDNN uses cointegration based Timedelay Neural network model proposed by Das (2019).First the cointegration of the data series is identified by Johansen cointegration test. Then Error correction model is fitted for the estimation of parameters i.e. Beta and Error coreection term (ECT).The estimated ECT is used as a auxiliary information in neural network fitting. Then the neural network model is used foresting of data series.}

\value{It returns the accuracy measures of the fitted cointegration based TDNN model.}

\references{
  %%
    Das (2019)<http://krishikosh.egranth.ac.in/handle/1/5810147805>
}

\author{
  Pankaj Das,Achal Lama, Girish Kumar Jha
}
\seealso{vars,urca, nnfor}
\examples{
data(finland)
data_example<-finland[,1:2]
ECTTDNN(data_example,"trace",0.8,1,5,5)
}
\keyword{Cointegration}
\keyword{Machine Learning}
\keyword{ECT}
\keyword{TDNN}
