dom_J <- 11

q_J <- questionnaire %>%
  select(id, dom_J) %>%
  rename(domanda = 2) %>%
  mutate(lower = str_to_lower(domanda)) %>%
  mutate(lower = str_replace_all(lower, " si ", " sì ")) %>%
  mutate(lower = str_replace_all(lower, "^si", "sì")) %>%
  mutate(lower = str_replace_all(lower, "[[:punct:]]", "")) %>%
  mutate(summary = ifelse(str_detect(lower, "sì"), "sì", ifelse(str_detect(lower, "\bno\b|^no"), "no", "altro")))

q_J_p <- q_J %>%
  count(summary, sort = T) %>%
  ggplot(aes(x = summary, y = n, fill = summary)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = n), vjust = + 1.5, size = 3.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(limits=c("sì", "no", "altro", NA))

print(q_J_p)

q_J_p_en <- q_J %>%
  count(summary, sort = T) %>%
  mutate(summary = ifelse(summary == "sì", "yes", ifelse(summary == "altro", "other", summary))) %>%
  ggplot(aes(x = summary, y = n, fill = summary)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = n), vjust = + 1.5, size = 3.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(limits=c("yes", "no", "other", NA))

print(q_J_p_en)

ggsave("plots/q_J_en.pdf")
ggsave("plots/q_J_en.png")

#############

dom_K <- 12

q_K <- questionnaire %>%
  select(id, dom_K) %>%
  rename(domanda = 2) %>%
  mutate(lower = str_to_lower(domanda)) %>%
  mutate(lower = str_replace_all(lower, " si ", " sì ")) %>%
  mutate(lower = str_replace_all(lower, "^si|si$", "sì")) %>%
  mutate(lower = str_replace_all(lower, "[[:punct:]]", "")) %>%
  mutate(summary = ifelse(str_detect(lower, "clienti|colleghi|fornitori"), "sì",
                          ifelse(str_detect(lower, "sì"), "sì", 
                          ifelse(str_detect(lower, "\bno\b|^no"), "no", "altro"))))
q_K %>%
  count(summary, sort = T)

q_K_p <- q_K %>%
  count(summary, sort = T) %>%
  ggplot(aes(x = summary, y = n, fill = summary)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = n), vjust = + 1.5, size = 3.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(limits=c("sì", "no", "altro", NA))

print(q_K_p)

ggsave("plots/q_K.pdf")
ggsave("plots/q_K.png")


q_K_p_en <- q_K %>%
  count(summary, sort = T) %>%
  mutate(summary = ifelse(summary == "sì", "yes", ifelse(summary == "altro", "other", summary))) %>%
  ggplot(aes(x = summary, y = n, fill = summary)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = n), vjust = + 1.5, size = 3.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(limits=c("yes", "no", "other", NA))

print(q_K_p_en)

ggsave("plots/q_K_en.pdf")
ggsave("plots/q_K_en.png")

############

dom_L <- 13

q_L <- questionnaire %>%
  select(id, dom_L) %>%
  rename(domanda = 2) %>%
  mutate(lower = str_to_lower(domanda)) %>%
  mutate(lower = str_replace_all(lower, " si ", " sì ")) %>%
  mutate(lower = str_replace_all(lower, "^si", "sì")) %>%
  mutate(lower = str_replace_all(lower, "[[:punct:]]", "")) %>%
  mutate(summary = ifelse(str_detect(lower, "sì"), "sì", ifelse(str_detect(lower, "\bno\b|^no"), "no", "altro")))
q_L %>%
  count(summary, sort = T)

q_L_p <- q_L %>%
  count(summary, sort = T) %>%
  ggplot(aes(x = summary, y = n, fill = summary)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = n), vjust = + 1.5, size = 3.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(limits=c("sì", "no", "altro", NA))

print(q_L_p)

ggsave("plots/q_L.pdf")
ggsave("plots/q_L.png")

q_L_p_en <- q_L %>%
  count(summary, sort = T) %>%
  mutate(summary = ifelse(summary == "sì", "yes", ifelse(summary == "altro", "other", summary))) %>%
  ggplot(aes(x = summary, y = n, fill = summary)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = n), vjust = + 1.5, size = 3.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(limits=c("yes", "no", "other", NA))

print(q_L_p_en)

ggsave("plots/q_L_en.pdf")
ggsave("plots/q_L_en.png")

###############


dom_I <- 10

q_I <- questionnaire %>%
  select(id, dom_I) %>%
  rename(domanda = 2) %>%
  mutate(lower = str_to_lower(domanda)) %>%
  mutate(summary = ifelse(str_detect(lower, "remoto|casa|smart|pc|computer"), "casa", "altro"))
                          #ifelse(str_detect(lower, "cambiato|diminuito|chiuso"), "cambiato", "altro")))
q_I %>%
  count(summary, sort = T)


q_I_p <- q_I %>%
  count(summary, sort = T) %>%
  ggplot(aes(x = summary, y = n, fill = summary)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = n), vjust = + 1.5, size = 3.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(limits=c("casa", "altro", NA))

print(q_I_p)

ggsave("plots/q_I.pdf")
ggsave("plots/q_I.png")

q_I_p_en <- q_I %>%
  count(summary, sort = T) %>%
  mutate(summary = ifelse(summary == "casa", "home", ifelse(summary == "altro", "other", summary))) %>%
  ggplot(aes(x = summary, y = n, fill = summary)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = n), vjust = + 1.5, size = 3.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(limits=c("home", "other", NA))

print(q_I_p_en)

ggsave("plots/q_I_en.pdf")
ggsave("plots/q_I_en.png")


###############

dom_V <- 23

q_V <- questionnaire %>%
  select(id, dom_V) %>%
  rename(domanda = 2) %>%
  mutate(lower = str_to_lower(domanda)) %>%
  mutate(lower = str_replace_all(lower, " si ", " sì ")) %>%
  mutate(lower = str_replace_all(lower, "^si$|si$", "sì")) %>%
  mutate(lower = ifelse(str_detect(lower, "spero"), "spero", lower)) %>%
  mutate(lower = ifelse(str_detect(lower, "forse"), "forse", lower)) %>%
  mutate(lower = ifelse(str_detect(lower, "non so|non lo so|non saprei"), "non so", lower)) %>%
  mutate(lower = ifelse(str_detect(lower, "certo|certamente|ovvio|sicuramente|assolutamente"), "sì", lower)) %>%
  mutate(summary = ifelse(str_detect(lower, "^sì$|^no$|^spero$|^forse$|^non so$", negate = T), "altro", lower))
q_V

q_V %>%
  count(summary, sort = T)


q_V_p <- q_V %>%
  count(summary, sort = T) %>%
  ggplot(aes(x = summary, y = n, fill = summary)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = n), vjust = + 1.5, size = 3.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(limits=c("sì", "no", "non so", "spero", "forse", "altro", NA))

print(q_V_p)

ggsave("plots/q_V.pdf")
ggsave("plots/q_V.png")

q_V_p_en <- q_V %>%
  count(summary, sort = T) %>%
  mutate(summary = ifelse(summary == "sì", "yes", 
                          ifelse(summary == "altro", "other", 
                                 ifelse(summary == "non so", "don't know", 
                                        ifelse(summary == "forse", "maybe", 
                                               ifelse(summary == "spero", "hope", summary)))))) %>%
  ggplot(aes(x = summary, y = n, fill = summary)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = n), vjust = + 1.5, size = 3.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(limits=c("yes", "no", "don't know", "hope", "maybe", "other", NA))

print(q_V_p_en)

ggsave("plots/q_V_en.pdf")
ggsave("plots/q_V_en.png")



###########

dom_W <- 24

q_W <- questionnaire %>%
  select(id, dom_W) %>%
  rename(domanda = 2) %>%
  mutate(lower = str_to_lower(domanda)) %>%
  mutate(lower = str_to_lower(domanda)) %>%
  mutate(lower = str_replace_all(lower, " si ", " sì ")) %>%
  mutate(lower = str_replace_all(lower, "^si$|si$", "sì")) %>%
  mutate(lower = ifelse(str_detect(lower, "spero"), "spero", lower)) %>%
  mutate(lower = ifelse(str_detect(lower, "forse"), "forse", lower)) %>%
  mutate(lower = ifelse(str_detect(lower, "non so|non lo so|non saprei"), "non so", lower)) %>%
  mutate(lower = ifelse(str_detect(lower, "certo|certamente|ovvio|sicuramente|assolutamente"), "sì", lower)) %>%
  mutate(summary = ifelse(str_detect(lower, "^sì$|^no$|^spero$|^forse$|^non so$", negate = T), "altro", lower))
q_W

q_W %>%
  count(domanda, sort = T)


q_V_p <- q_V %>%
  count(summary, sort = T) %>%
  ggplot(aes(x = summary, y = n, fill = summary)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_text(aes(label = n), vjust = + 1.5, size = 3.5) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(limits=c("sì", "no", "non so", "spero", "forse", "altro", NA))

print(q_V_p)

ggsave("plots/q_V.pdf")
ggsave("plots/q_V.png")
