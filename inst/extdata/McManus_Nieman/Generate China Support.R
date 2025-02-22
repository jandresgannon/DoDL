### Signaling Latent Variable
### Major Powers
### McManus and Nieman

library(MCMCpack)
library(doBy)
set.seed(5)


## China signal
d <- read.csv("Individual Signals Dataset.csv", header=TRUE)
row.names(d) <- d$ctry_yr

d$chi_pact <- ordered(as.factor(d$chi_pact))
d$chi_vis <- ordered(as.factor(d$chi_vis))
d$chi_ex <- ordered(as.factor(d$chi_ex))
d$yr1950 <- ordered(as.factor(d$yr1950))
d$yr1951 <- ordered(as.factor(d$yr1951))
d$yr1952 <- ordered(as.factor(d$yr1952))
d$yr1953 <- ordered(as.factor(d$yr1953))
d$yr1954 <- ordered(as.factor(d$yr1954))
d$yr1955 <- ordered(as.factor(d$yr1955))
d$yr1956 <- ordered(as.factor(d$yr1956))
d$yr1957 <- ordered(as.factor(d$yr1957))
d$yr1958 <- ordered(as.factor(d$yr1958))
d$yr1959 <- ordered(as.factor(d$yr1959))
d$yr1960 <- ordered(as.factor(d$yr1960))
d$yr1961 <- ordered(as.factor(d$yr1961))
d$yr1962 <- ordered(as.factor(d$yr1962))
d$yr1963 <- ordered(as.factor(d$yr1963))
d$yr1964 <- ordered(as.factor(d$yr1964))
d$yr1965 <- ordered(as.factor(d$yr1965))
d$yr1966 <- ordered(as.factor(d$yr1966))
d$yr1967 <- ordered(as.factor(d$yr1967))
d$yr1968 <- ordered(as.factor(d$yr1968))
d$yr1969 <- ordered(as.factor(d$yr1969))
d$yr1970 <- ordered(as.factor(d$yr1970))
d$yr1971 <- ordered(as.factor(d$yr1971))
d$yr1972 <- ordered(as.factor(d$yr1972))
d$yr1973 <- ordered(as.factor(d$yr1973))
d$yr1974 <- ordered(as.factor(d$yr1974))
d$yr1975 <- ordered(as.factor(d$yr1975))
d$yr1976 <- ordered(as.factor(d$yr1976))
d$yr1977 <- ordered(as.factor(d$yr1977))
d$yr1978 <- ordered(as.factor(d$yr1978))
d$yr1979 <- ordered(as.factor(d$yr1979))
d$yr1980 <- ordered(as.factor(d$yr1980))
d$yr1981 <- ordered(as.factor(d$yr1981))
d$yr1982 <- ordered(as.factor(d$yr1982))
d$yr1983 <- ordered(as.factor(d$yr1983))
d$yr1984 <- ordered(as.factor(d$yr1984))
d$yr1985 <- ordered(as.factor(d$yr1985))
d$yr1986 <- ordered(as.factor(d$yr1986))
d$yr1987 <- ordered(as.factor(d$yr1987))
d$yr1988 <- ordered(as.factor(d$yr1988))
d$yr1989 <- ordered(as.factor(d$yr1989))
d$yr1990 <- ordered(as.factor(d$yr1990))
d$yr1991 <- ordered(as.factor(d$yr1991))
d$yr1992 <- ordered(as.factor(d$yr1992))
d$yr1993 <- ordered(as.factor(d$yr1993))
d$yr1994 <- ordered(as.factor(d$yr1994))
d$yr1995 <- ordered(as.factor(d$yr1995))
d$yr1996 <- ordered(as.factor(d$yr1996))
d$yr1997 <- ordered(as.factor(d$yr1997))
d$yr1998 <- ordered(as.factor(d$yr1998))
d$yr1999 <- ordered(as.factor(d$yr1999))
d$yr2000 <- ordered(as.factor(d$yr2000))
d$yr2001 <- ordered(as.factor(d$yr2001))
d$yr2002 <- ordered(as.factor(d$yr2002))
d$yr2003 <- ordered(as.factor(d$yr2003))
d$yr2004 <- ordered(as.factor(d$yr2004))
d$yr2005 <- ordered(as.factor(d$yr2005))
d$yr2006 <- ordered(as.factor(d$yr2006))
d$yr2007 <- ordered(as.factor(d$yr2007))
d$yr2008 <- ordered(as.factor(d$yr2008))
d$yr2009 <- ordered(as.factor(d$yr2009))
d$yr2010 <- ordered(as.factor(d$yr2010))
d$yr2011 <- ordered(as.factor(d$yr2011))
d$yr2012 <- ordered(as.factor(d$yr2012))

# Estimate model
post_chi <- MCMCmixfactanal(~
chi_pact+log_chi_arms+log_chi_troops+chi_vis+chi_ex
+yr1950+yr1951+yr1952+yr1953+yr1954+yr1955+yr1956+yr1957+yr1958+yr1959
+yr1960+yr1961+yr1962+yr1963+yr1964+yr1965+yr1966+yr1967+yr1968+yr1969
+yr1970+yr1971+yr1972+yr1973+yr1974+yr1975+yr1976+yr1977+yr1978+yr1979
+yr1980+yr1981+yr1982+yr1983+yr1984+yr1985+yr1986+yr1987+yr1988+yr1989
+yr1990+yr1991+yr1992+yr1993+yr1994+yr1995+yr1996+yr1997+yr1998+yr1999
+yr2000+yr2001+yr2002+yr2003+yr2004+yr2005+yr2006+yr2007+yr2008+yr2009
+yr2010+yr2011+yr2012,
factors=1, data=d,
lambda.constraints = list(chi_pact=list(2,"+")),
burnin=20000, mcmc=100000, thin=50,
verbose=500, L0=.25, store.lambda=TRUE,
store.scores=TRUE, tune=1.2)

summary(post_chi)

# convert object from mcmc to data.frame and transpose
c<-as.data.frame(post_chi)
c<-t(c)
c1<-apply(c,1,quantile,probs = c(0.025, 0.05, 0.5, 0.95, 0.975))
mean<-apply(c,1,mean)
sd<-apply(c,1,sd)
c1<-rbind(c1,mean,sd)
c1<-t(c1)

# isolate and extract lambda/psi (beginning and end of variables)
lambda<-c1[1:134,]
n<-nrow(c1)
psi<-c1[(n-1):n,]
lambda<-rbind(lambda,psi)

# isolate country ideal points (phi; in middle)
phi<-c1[135:(n-2),]

# change vars to lower 95+ 90+ median+ and upper 90+ 95; change names to remove ".2"
lambda
colnames(lambda) <- c("low95","low90","median","high90","high95","mean","sd")

# change vars to lower 95+ median+ and upper 90; change names to leave just country name
phi
colnames(phi) <- c("low95","low90","median","high90","high95","mean","sd")
rownames(phi) <- sub("\\phi.", "", rownames(phi))

# Save lambda/psi as one .csv file and save country ideal points as another .csv file
write.csv(lambda,file="lambda_chi.csv")
write.csv(phi,file="country_irt_chi.csv")


