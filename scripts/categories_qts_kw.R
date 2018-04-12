# aggregate all "spiritual" experiences
d_spirit <- d %>%
  mutate_at(vars(godviapeople, godviascript, godviamind, godvoxaloud, 
                 godcommpics, godviavisions, godviadreams, godguideviaknowing, 
                 godguideviasensations, godsenseplaceinbody, godviatouch, 
                 godviasmell, godexpviaawe, neartangiblegod, presencenotgod, 
                 presencedemon, beingentbody, seehearnotgod, whitelight, 
                 trmblshakespirtpwr, rushofspiritpwr, intenseemospiritpwr, 
                 timeslowpray, mindspiritexitbody, spiritbeingencounter),
            funs(. %>% as.numeric() %>% - 1)) %>%
  gather(question, response,
         c(godviapeople, godviascript, godviamind, godvoxaloud, 
           godcommpics, godviavisions, godviadreams, godguideviaknowing, 
           godguideviasensations, godsenseplaceinbody, godviatouch, 
           godviasmell, godexpviaawe, neartangiblegod, presencenotgod, 
           presencedemon, beingentbody, seehearnotgod, whitelight, 
           trmblshakespirtpwr, rushofspiritpwr, intenseemospiritpwr, 
           timeslowpray, mindspiritexitbody, spiritbeingencounter)) %>%
  group_by(question) %>%
  mutate(max_response = max(response, na.rm = T),
         response_norm = response/max_response)

# aggregate experiences by sensory modality
d_sense <- d %>%
  mutate_at(vars(godguideviasensations, godviabodyexperiences, godviatouch,
                 trmblshakespirtpwr, rushofspiritpwr, mindspiritexitbody, 
                 neartangiblegod, godviamind, godvoxaloud, godcommpics, 
                 godviavisions, godviadreams, godguideviaknowing, godviasmell),
            funs(. %>% as.numeric() %>% -1)) %>%
  gather(question, response,
         c(godguideviasensations, godviabodyexperiences, godviatouch,
           trmblshakespirtpwr, rushofspiritpwr, mindspiritexitbody, 
           neartangiblegod, godviamind, godvoxaloud, godcommpics, godviavisions,
           godviadreams, godguideviaknowing, godviasmell)) %>%
  group_by(question) %>%
  mutate(max_response = max(response, na.rm = T),
         response_norm = response/max_response,
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
           levels = c("Image", "Voice", "Tactile", "Smell", "Presence", "Other")))

