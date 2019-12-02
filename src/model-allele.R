# At this point (11/20), I think it would be best to just get some proof of
# concept for this sort of thing. What I want to do is build up a model to
# simulate sex-specific allele frequencies and how there could be some
# differential frequency of some allele in females when compared to males. In
# the specific case that I would like to investigate (white-tailed deer
# population in CWD endemic area), there are many different alleles or
# haplotypes that confer differential success for survival (and, therefore,
# fitness). Males are also subject to increased mortality (which is random) via
# hunting as well. For now, we can abstract the many different alleles to two
# different alleles: susceptible and tolerant. 

# pop_size - total population size
# Half male and female
pop_size <- 100000
female_pop <- 0.5 * pop_size
male_pop <- 0.5 * pop_size
# Initialize allele frequencies
# I will assume that the tolerant type starts off at a very low frequency 
s.freq <- 0.99
t.freq <- 0.01
# Initialize birth/death rates
# May need to configure this a bit...
birth <- 0.5
death <- 0.5

simulate.model <- function (pop.size, b, d, allele1, allele2) {

}