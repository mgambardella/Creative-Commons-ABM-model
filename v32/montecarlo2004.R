set.population <- function(n,
                           character = expand.grid(
                             famous = c(TRUE, FALSE),
                             com = c(TRUE, FALSE),
                             maturity = 1:10),
                           components = list(own,effort,notoriety,diffusion,control,
                             status,skip), ...) {
  m <- length(components);
  
  crowd <- gen(Participant, n, 
               characteristics = list(famous = TRUE, id = "none",
                 com = TRUE, rationality = 100, maturity = 1),
               preferences = list(factors = components,
                 a = rep(1/m, m)));
  
  character.space <- 1:nrow(character);
  demographics <- sample(character.space, n, ...);
  
  for(i in 1:n) {
    spec <- character[demographics[i],];
    crowd[[i]]$characteristics[names(spec)] <- spec;
    crowd[[i]]$characteristics$id <- paste("p", i, sep="");
    names(crowd[[i]]$preferences$a) <- paste("a", 1:m, sep="");
  }
  
  names(crowd) <- paste("p", 1:n, sep="");
  return(crowd);
}

############################

run.irace <- function(population = set.population(100), 
                      n.instances = 1000,
                      hook.fun = hook.run.extra,
                      budget = 1000,
                      log.file = "",
                      hook.eval = evaluate,
                      objective = c(1,1,2,0,8,0),
                      store.base = list(set.store("empty"))) {

  return(irace(tunerConfig = list(
                 hookRun = hook.fun,
                 instances = sample(store.base, n.instances, 
                   replace = TRUE),
                 maxExperiments = budget,
                 instances.extra.params = lapply(1:n.instances, 
                   function(x) 
                   list(eval = hook.eval,
                        target = objective,
                        population = population)),
                 logFile = log.file),
                 parameters = set.parameters(length(population), 
                   length(population[[1]]$preferences$a))));
}

###############################

my.participants <- set.population(100,
                                  expand.grid(famous=c(T,F),
                                              maturity=c(1,2,3,4,5,6,7,8,9,10), com=c(T,F)),replace = TRUE, 
                                  list(own,effort,notoriety,diffusion,control,
                             status,skip));
test2 <- run.irace(my.participants, budget=1000);
scenari2 <- instantiate.model(test2, population = my.participants);

###############################

m2 <- scenari2[[1]]$copy();
m2.output <- m2$run(100);
m2.idea.author.ids <-  t(sapply(m2.output$element, 
                                function(e) e$meta$author$id));
summary(as.factor(m2.idea.author.ids));

################################

test <- function(platform, steps, times = 10, initial.store = list(), ...) {
  return(sapply(1:times,
                function(i) {
                  platform$store <- initial.store;
                  return(wrap(platform$run(steps), ...));
                }));
}

wrap <- function(output, participant.names) {
  if(length(output$step) > 0) {
    return(summary(factor(sapply(output$element, 
                                 function(x) {
                                   x$meta$author$id;
                                 }), levels = participant.names)));
  } else {
    return(invisible(numeric(length(participant.names))));
  }
}

#################################

mc2 <- test(platform=m2, steps=test2$s[1], times=10, 
            initial.store = m2$store,
            participant.names = names(m2$population));
			
# to latex
require(xtable);
print(xtable(t(mc2), floating=FALSE, file="mc2.tex"));

##################################

write.model.results <- function(test.results, model, ...) {
  write.csv(cbind(t(sapply(model$population, function(a) c(unlist(a$characteristics), a$preferences$a))), test.results), ...);
}

write.model.results(mc2, m2, file="mc2.csv");

###################################
