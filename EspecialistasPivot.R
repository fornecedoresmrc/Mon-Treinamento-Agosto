library(tidyverse)

Especialistas <- read_excel("~/R/Treino/base_estatistica.xlsx", sheet = "Sheet2")

SubTable <- Especialistas %>%
select(Matriz,
       Cliente,
       'Milho e Sorgo',
       Soja,
       "Cidade Matriz(correto)",
       Glifosato,
       APS,
       Baixas,
       'UF Matriz (cadastro atual)',
       mEspecialista)

str(SubTable)

SubTable <- SubTable %>%
  mutate(Faturamento = Soja  +Glifosato + `Milho e Sorgo`,
          Matriz = as.factor(Matriz),
         Cliente = as.factor(Cliente))

Especialistas <- Especialistas %>%
  mutate(Faturamento = Soja  +Glifosato + `Milho e Sorgo`,
         Matriz = as.factor(Matriz),
         Cliente = as.factor(Cliente))

ComecoPivot1 <- SubTable %>%
  group_by(Cliente,mEspecialista) %>%
  summarise(
    #Conta_Matriz = length(unique(Matriz)),
    #Conta_Clientes = length(unique(Cliente)),
            Soma_Faturamento = sum(Faturamento, na.rm = TRUE),
            Soma_Baixas = sum(Baixas, na.rm = TRUE))


ggplot(data = filter(ComecoPivot1,Soma_Baixas < 500)) +
  aes(Soma_Faturamento,
      Soma_Baixas,
      color = mEspecialista) +
      facet_grid(mEspecialista ~ .) +
  geom_point()
  

names(Certo)

Certo <- Especialistas %>% filter(Baixas < 500)

LM <- lm(Faturamento ~ Baixas + mEspecialista + Area,Certo)




