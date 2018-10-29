### hisafe-calibration
### GA PREP
### Author: Kevin J. Wolz

ga.path    <- "/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe-GA/"
input.path <- "/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe-calibration/raw_data/"
default.path <- paste0(ga.path, "default_folders/")
dir.create(default.path, showWarnings = FALSE, recursive = TRUE)

A3 <- define_hisafe(path     = default.path,
                    profiles = "all-private",
                    template = "restinclieres_agroforestry_A3",
                    SimulationName = "A3",
                    weatherFile    = A3.WEATHER)

A2 <- define_hisafe(path     = default.path,
                    profiles = "all-private",
                    template = "restinclieres_agroforestry_A2",
                    SimulationName = "A2",
                    weatherFile    = A2.WEATHER)

A4 <- define_hisafe(path     = default.path,
                    profiles = "all-private",
                    template = "restinclieres_forestry_A4",
                    SimulationName = "A4",
                    weatherFile    = A4.WEATHER)

A2.MC <- define_hisafe(path     = default.path,
                       profiles = "all-private",
                       template = "restinclieres_monocrop_A2",
                       SimulationName = "A2MC",
                       weatherFile    = A2.WEATHER)


##### BULID DEFAULT FOLDERS #####
default.files <- c("tec", "plt", "pro", "par", "pld", "wth")

build_hisafe(hip           = A3,
             files         = default.files,
             plot.scene    = FALSE,
             summary.files = FALSE)
dum <- file.copy(from = paste0(input.path,   "A3.rootcal"),
                 to   = paste0(default.path, "A3/A3.rootcal"))
dum <- file.copy(from = paste0(input.path,   "A3.dbhcal"),
                 to   = paste0(default.path, "A3/A3.dbhcal"))

build_hisafe(hip           = A2,
             files         = default.files,
             plot.scene    = FALSE,
             summary.files = FALSE)
dum <- file.copy(from = paste0(input.path,   "A2.dbhcal"),
                 to   = paste0(default.path, "A2/A2.dbhcal"))

build_hisafe(hip           = A4,
             files         = default.files,
             plot.scene    = FALSE,
             summary.files = FALSE)
dum <- file.copy(from = paste0(input.path,   "A4.dbhcal"),
                 to   = paste0(default.path, "A4/A4.dbhcal"))

build_hisafe(hip           = A2.MC,
             files         = default.files,
             plot.scene    = FALSE,
             summary.files = FALSE)

##### BULID TEMPLATE FOLDERS #####
template.path <- paste0(ga.path, "templates/")
dir.create(template.path, showWarnings = FALSE, recursive = TRUE)

A2$path <- A3$path <- A4$path <- A2.MC$path <- template.path

A3$profiles    <- c("annualDBH")
A2$profiles    <- c("annualDBH", "annualCellsYield")
A4$profiles    <- c("annualDBH")
A2.MC$profiles <- c("annualCellsYield")

template.files <- c("sim", "tree")

build_hisafe(hip           = A3,
             files         = template.files,
             plot.scene    = FALSE,
             summary.files = FALSE)

build_hisafe(hip           = A2,
             files         = template.files,
             plot.scene    = FALSE,
             summary.files = FALSE)

build_hisafe(hip           = A4,
             files         = template.files,
             plot.scene    = FALSE,
             summary.files = FALSE)

build_hisafe(hip           = A2.MC,
             files         = "sim",
             plot.scene    = FALSE,
             summary.files = FALSE)


##### CREATE INITIAL POP FOR REOUND 2 #####
pop.archive1 <- read_csv("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/SUPPORT/hisafe-GA-ROUND1/output/pop_archive.csv")

new.init.pop <- pop.archive1 %>%
  filter(A2 != A3 & A2 != A4) %>%
  distinct(gen.id, .keep_all = TRUE) %>%
  arrange(VOL) %>%
  filter(VOL < 23)# %>%
  #select(lueMax:treeMinTranspirationPotential)

#write_csv(new.init.pop, "/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe-GA/input/INITIAL_POP.csv")
# new.init.pop %>%
#   select(orig.gen, gen.id) %>%
#   dplyr::mutate(gen = 27, id = 1:nrow(.)) %>%
#   write_csv("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/SUPPORT/hisafe-GA-ROUND1/round2_initial_pop_info.csv")


end.GA1.pop <- pop.archive1 %>%
  filter(gen == max(gen))

init.surv <- read_csv("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe-GA/inital_gen_surv_ids.csv")
nrow(init.surv)

hist(end.GA1.pop$orig.gen) #end of GA1 orig hist
hist(new.init.pop$orig.gen[init.surv$gen.id]) #end of GA2 first round of selection hist
