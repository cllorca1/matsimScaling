#plot additional tests regarding the low capacity link simulation

all$exponent[all$exponent=="tests.50-1.00"] = .5
all$exponent[all$exponent=="tests.75-1.00"] = 0.75
all$exponent[all$exponent=="tests1.00-1.00"] = 1 
all$exponent[all$exponent=="tests1.25-1.00"] = 1.25
all$exponent[all$exponent=="tests1.50-1.00"] = 1.5
all$exponent[all$exponent=="tests1.75-1.00"] = 1.75
all$exponent[all$exponent=="tests2.00-1.00"] = 2
all$exponent[all$exponent=="tests2.25-1.00"] = 2.25
all$exponent[all$exponent=="tests2.55-1.00"] = 1.5
all$exponent[all$exponent=="tests2.75-1.00"] = 2.75
all$exponent[all$exponent=="tests3.25-1.00"] = 3.25
all$exponent[all$exponent=="tests4.00-1.00"] = 4.00

all$exponent[all$exponent=="testsUpstream10K.50-1.00"] = 0.5
all$exponent[all$exponent=="testsUpstream10K1.00-1.00"] = 1
all$exponent[all$exponent=="testsUpstream10K1.50-1.00"] = 1.5
all$exponent[all$exponent=="testsUpstream10K2.00-1.00"] = 2
all$exponent[all$exponent=="testsUpstream10K2.50-1.00"] = 2.5
all$exponent[all$exponent=="testsUpstream10K3.00-1.00"] = 3



all$flow = as.numeric(all$exponent) * all$capacity



ggplot(all, aes(x = flow, y = speed, group = as.factor(capacity), color = as.factor(capacity))) +
  geom_line() +
  geom_point(size = 3, shape = 21, fill = "white") 
  
  

