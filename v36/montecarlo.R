write.csv(t(sapply(pop03, function(guy) { 
	list(forprofit=guy$characteristics$com,
		maturity=guy$characteristics$maturity,
		famous=guy$characteristics$famous)
		})), 
		file="population.csv")

source("validation.R")

#### 2003 ####
m03 <- seed.model.from.file("replication-2003", all=TRUE, run=FALSE);
tab03 <- read.csv("replication-2003.csv");
steps1 <- tab03$s[1];
mc03 <- test.raw(m03[[1]], n = steps1);
m03.1.output <- sapply(mc03, get.author.openness.sums, m03[[1]]);
write.csv(m03.1.output, file="m03.1.output.csv");

#### 2004 ####
m04 <- seed.model.from.file("replication-2004", all=TRUE, run=FALSE);
tab04 <- read.csv("replication-2004.csv");
steps1 <- tab04$s[1];
mc04 <- test.raw(m04[[1]], n = steps1);
m04.1.output <- sapply(mc04, get.author.openness.sums, m04[[1]]);
write.csv(m04.1.output, file="m04.1.output.csv");

#### 2005 ####
m05 <- seed.model.from.file("replication-2005", all=TRUE, run=FALSE);
tab05 <- read.csv("replication-2005.csv");
steps1 <- tab05$s[1];
mc05 <- test.raw(m05[[1]], n = steps1);
m05.1.output <- sapply(mc05, get.author.openness.sums, m05[[1]]);
write.csv(m05.1.output, file="m05.1.output.csv");

#### 2006 ####
m06 <- seed.model.from.file("replication-2006", all=TRUE, run=FALSE);
tab06 <- read.csv("replication-2006.csv");
steps1 <- tab06$s[1];
mc06 <- test.raw(m06[[1]], n = steps1);
m06.1.output <- sapply(mc06, get.author.openness.sums, m06[[1]]);
write.csv(m06.1.output, file="m06.1.output.csv");

#### 2007 ####
m07 <- seed.model.from.file("replication-2007", all=TRUE, run=FALSE);
tab07 <- read.csv("replication-2007.csv");
steps1 <- tab07$s[1];
mc07 <- test.raw(m07[[1]], n = steps1);
m07.1.output <- sapply(mc07, get.author.openness.sums, m07[[1]]);
write.csv(m07.1.output, file="m07.1.output.csv");

#### 2008 ####
m08 <- seed.model.from.file("replication-2008", all=TRUE, run=FALSE);
tab08 <- read.csv("replication-2008.csv");
steps1 <- tab08$s[1];
mc08 <- test.raw(m08[[1]], n = steps1);
m08.1.output <- sapply(mc08, get.author.openness.sums, m08[[1]]);
write.csv(m08.1.output, file="m08.1.output.csv");

#### 2009 ####
m09 <- seed.model.from.file("replication-2009", all=TRUE, run=FALSE);
tab09 <- read.csv("replication-2009.csv");
steps1 <- tab09$s[1];
mc09 <- test.raw(m09[[1]], n = steps1);
m09.1.output <- sapply(mc09, get.author.openness.sums, m09[[1]]);
write.csv(m09.1.output, file="m09.1.output.csv");