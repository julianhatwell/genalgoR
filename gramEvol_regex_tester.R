library(gramEvol)
library(rex)

re <- "^(\\+|-)?[[:digit:]]+(\\.[[:digit:]]+)?$"
grepl(re, "+1.1")
grepl(re, "1+1")

matching <- c("1", "11.1"
              , "1.11", "+11"
              , "-11", "-11.1")
non.matching <- c("a", "1.", "1..1"
                  , "-.1", "-", "1-"
                  , "1.-1", ".-1"
                  , "1.-", "1.1.1"
                  , "", ".", "1.1-"
                  , "11-11")

re.score <- function(re) { 
  score <- sum(sapply(matching
                    , function(x) { 
                        grepl(re, x)
                        }
                      )) + 
            sum(sapply(non.matching
                    , function(x) {
                       !grepl(re, x)
                       }
                      )) 
  return (length(matching) + length(non.matching) - score)
}

fitfunc <- function(expr) {
  re.score(eval(expr))
}

grammarDef <- CreateGrammar(list(
  re = grule(rex(start, rules, end))
  , rules = grule(rule, .(rule, rules))
  , rule = grule(numbers, ".", or("+", "-"), maybe(rules))))
grammarDef

GrammarRandomExpression(grammarDef)
GrammaticalEvolution(grammarDef, fitfunc, max.depth = 7, terminationCost = 0)
