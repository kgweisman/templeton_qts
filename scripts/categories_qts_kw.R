# aggregate all "spiritual" experiences
d_spirit <- d %>%
  # mutate_at(vars(godviapeople, godviascript, godviamind, godvoxaloud, 
  #                godcommpics, godviavisions, godviadreams, godguideviaknowing, 
  #                godguideviasensations, godsenseplaceinbody, godviatouch, 
  #                godviasmell, # godexpviaawe, 
  #                neartangiblegod, presencenotgod, 
  #                presencedemon, beingentbody, seehearnotgod, whitelight, 
  #                trmblshakespirtpwr, rushofspiritpwr, intenseemospiritpwr, 
  #                timeslowpray, mindspiritexitbody, spiritbeingencounter),
  #           funs(. %>% as.numeric() %>% - 1)) %>%
  gather(question, response,
         c(godviapeople, godviascript, godviamind, godvoxaloud, 
           godcommpics, godviavisions, godviadreams, godguideviaknowing, 
           godguideviasensations, godsenseplaceinbody, godviatouch, 
           godviasmell, # godexpviaawe, 
           neartangiblegod, presencenotgod, 
           presencedemon, beingentbody, seehearnotgod, whitelight, 
           trmblshakespirtpwr, rushofspiritpwr, intenseemospiritpwr, 
           timeslowpray, mindspiritexitbody, spiritbeingencounter)) %>%
  filter(!response %in% c("Other", "NA", " ", ""), !is.na(response)) %>%
  mutate(response = recode(response,
                           "No" = 0,
                           "Maybe" = 0.5,
                           "Yes" = 1)) %>%
  group_by(question) %>%
  # mutate(max_response = max(response, na.rm = T),
  #        response_norm = response/max_response) %>%
  ungroup() %>%
  distinct() %>%
  group_by(subject_name) %>%
  mutate(spex_score = sum(response, na.rm = T)) %>%
  ungroup() %>%
  distinct()

# aggregate experiences by sensory modality
d_sense <- d %>%
  # mutate_at(vars(godguideviasensations, godviabodyexperiences, godviatouch,
  #                trmblshakespirtpwr, rushofspiritpwr, mindspiritexitbody, 
  #                neartangiblegod, godviamind, godvoxaloud, godcommpics, 
  #                godviavisions, godviadreams, godguideviaknowing, godviasmell),
  #           funs(. %>% as.numeric() %>% -1)) %>%
  gather(question, response,
         c(godguideviasensations, godviabodyexperiences, godviatouch,
           trmblshakespirtpwr, rushofspiritpwr, mindspiritexitbody, 
           neartangiblegod, godviamind, godvoxaloud, godcommpics, godviavisions,
           godviadreams, godguideviaknowing, godviasmell)) %>%
  filter(!response %in% c("Other", "NA", " ", ""), !is.na(response)) %>%
  mutate(response = recode(response,
                           "No" = 0,
                           "Maybe" = 0.5,
                           "Yes" = 1)) %>%
  group_by(question) %>%
  mutate(#max_response = max(response, na.rm = T),
         #response_norm = response/max_response,
         sense = factor(case_when(
           question %in% c("godguideviasensations", "godviabodyexperiences",
                           "godviatouch", "trmblshakespirtpwr", 
                           "rushofspiritpwr", "mindspiritexitbody", 
                           "neartangiblegod") ~ "Tactile",
           question %in% c("godviamind", "godvoxaloud") ~ "Voice",
           question %in% c("godcommpics", "godviavisions", "godviadreams") ~ 
             "Image",
           question %in% c("godviasmell") ~ "Smell",
           question %in% c("neartangiblegod", "presencenotgod", "presencedemon", 
                           "seehearnotgod", "spiritbeingencounter") ~ "Presence",
           question %in% c("godguideviaknowing") ~ "Other"),
           levels = c("Image", "Voice", "Tactile", "Smell", 
                      "Presence", "Other"))) %>%
  ungroup() %>%
  distinct() %>%
  group_by(subject_name, sense) %>%
  mutate(sense_score = sum(response, na.rm = T)) %>%
  ungroup() %>%
  distinct()
