# Fix names of algal data


alg_df <- read_csv(here("data/algae_spp_all.csv"))


alg_names <- alg_df|>distinct(ShortName, Species)

multiples <- alg_names|>group_by(ShortName)|>count()|>filter(n>1)|>pull(ShortName)
multiples2 <- alg_names|>group_by(Species)|>count()|>filter(n>1)|>pull(Species) # only doubled up shortnames

multiples_named <- alg_names|>filter(ShortName %in% multiples)

alg_df_resolved <- alg_df|>
  mutate(Species = case_when(ShortName == "ECsilesi" ~ "Encyonema silesiacum",
                             ShortName == "FRcapuci" ~ "Fragilaria capucina",
                             ShortName == "NIsinde" ~ "Grunowia solgensis",
                             ShortName == "NIsintab" ~ "Grunowia tabellaria",
                             ShortName == "SUtients" ~ "Surirella tientsinensis", 
                             .default = Species))



multiples_resolved <- alg_df_resolved|>distinct(ShortName, Species)|>group_by(Species)|>count()|>filter(n>1)|>pull(Species) #zero for both short and species names

alg_df_resolved|>write_csv(here("data/alg_df_resolved.csv"))

