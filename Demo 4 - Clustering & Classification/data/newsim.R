

library(tidyverse)
set.seed(1337)

orders <- read_csv2('data/orders.csv')

RFM <- orders %>% 
    mutate_at(vars(CustomerID), as.factor) %>% 
    group_by(CustomerID) %>% 
    summarise(R = as.integer(Sys.Date() - max(Date)),
              F = n(),
              M = sum(OrderTotal))
cluster <- RFM %>% 
    select(-CustomerID) %>% 
    kmeans(2) 

RFM_clust <- RFM %>% 
    mutate(cluster = as.factor(cluster$cluster))

RFM_clust %>% 
    ggplot(aes(x = R, y = F, size = M,color = cluster)) + 
    geom_jitter(alpha = .2) + 
    #scale_size(range=c(1,10)) +
    labs(title = 'Cluster allocation and RFM metrics')

## Simulating new transactions and adding to former
# ----------------------------------------------------------- 
# -----------------------------------------------------------

orders_clust <- orders %>% 
    mutate_at(vars(CustomerID), as.factor) %>% 
    inner_join(RFM_clust) %>% 
    select(TransactionID:OrderTotal, cluster)

N <- 1000
frac <- .5
id_start <- 1091

new_cust <- seq(1091, by = 1, length.out = N) %>% sample(N)

cust <- orders %>% pull(CustomerID) %>% unique() %>% sort()
freq <- RFM %>% pull(F) %>% sample(N, replace=T) %>% 
    magrittr::add(rnorm(N, 0, 2)) %>% round() %>% abs()

new_trans <- list()
for(k in 1:N){
    thisclust <- as.character(rbinom(1, 1, frac) + 1)
    new_trans[[k]] <- orders_clust %>% 
            filter(cluster == thisclust) %>% 
            select(-cluster) %>% 
            sample_n(freq[k]) %>% 
            mutate(CustomerID = new_cust[k])
}

orders <- bind_rows(new_trans) %>% 
    bind_rows(orders)

write.csv2(orders, 'data/orders_sim.csv', row.names = F)
# ----------------------------------------------------------- 
# -----------------------------------------------------------


###### ZWEITER ANSATZ: Customer Tabelle selbst bauen

# Simulating customer dataset
cl_tab <- RFM_clust$cluster %>% table

n1 <- cl_tab[1]
n2 <- cl_tab[2]

cust1 <- RFM_clust %>% filter(cluster == "1") %>% pull(CustomerID)

M1 <- c(rep('Divorced', 2),
        rep('Married', 7),
        rep('Single', 1))

S1 <- c(rep('Male', 7),
        rep('Female', 3))

E1 <- c(rep('Bachelor', 2),
        rep('Master', 1),
        rep('High School', 7))

A1 <- rnorm(mean = 55, sd = 20, n = n1) %>% round()

C1 <- tibble(
    CustomerID = cust1,
    Age = A1,
    MaritalStatus = sample(M1, n1, replace = T),
    Sex = sample(S1, n1, replace = T),
    Education = sample(E1, n1, replace = T)
)

cust2 <- RFM_clust %>% filter(cluster == "2") %>% pull(CustomerID)

M2 <- c(rep('Divorced', 4),
        rep('Married', 1),
        rep('Single', 5))

S2 <- c(rep('Male', 3),
        rep('Female', 7))

E2 <- c(rep('Bachelor', 3),
        rep('Master', 6),
        rep('High School', 1))

A2 <- rnorm(mean = 35, sd = 10, n = n2) %>% round()

C2 <- tibble(
    CustomerID = cust2,
    Age = A2,
    MaritalStatus = sample(M2, n2, replace = T),
    Sex = sample(S2, n2, replace = T),
    Education = sample(E2, n2, replace = T)
)

new_modeldat <- bind_rows(C1, C2) %>%
    inner_join(RFM_clust %>% select(CustomerID, cluster)) %>% 
    select(-cluster)

write.csv2(new_modeldat, file='data/customers_sim.csv', row.names = F)
