## set.seed(1001) # setting the random number generator
L <- 30 # maximum range
nparents <- 10 # number of starting parents
offspr_per_parent <- 100 # yup
noffspr <- nparents * offspr_per_parent # total offspring
dispdist <- 2 # dispersal distance

parent_x <- runif(nparents, min = 0, max = L) # parent x value from uniform distribution
parent_y <- runif(nparents, min = 0, max = L) # parent y value from uinform distribution

parent_xy <- data.frame(parent_x=parent_x, parent_y=parent_y)

# Could be a lot less resource intensive #
angle <- runif(noffspr, min = 0, max = 2 * pi) # angle of dispersal from uniform distribution
dist <- rexp(noffspr, 1/dispdist) # distance of dispersal from exponential distribution (average at 1/2)

offspr_x <- rep(parent_x, each = offspr_per_parent) + cos(angle) * dist # offspring x location: repeat parent x value for each offspring + cosine angle times distance
offspr_y <- rep(parent_y, each = offspr_per_parent) + sin(angle) * dist # same as before

offspr_pos <- data.frame(offspr_x=offspr_x, offspr_y=offspr_y) # binding the x and y values together

### Fungal Dispersal ###
f_colonies <- 5
f_spores <- 200 # it's probably a lot more(?)
f_dispdist <- .5
p_infection <- 0.25
f_movedist <- 5

# Assuming the fungal pathogens depend on parent trees to survive?
fparent_xy <- parent_xy[sample(nrow(parent_xy), f_colonies), ] # starting location of fungi is on existing parent trees
#names(fparent_xy)[names(fparent_xy)=="x"] <- "fparent_x"
#names(fparent_xy)[names(fparent_xy)=="y"] <- "fparent_y"  # renaming the columns for consistency

fangle <- runif(f_spores, min = 0, max = 2*pi) # list of angles to shoot off the spores
fdist <- rexp(f_spores, 1/f_dispdist) # distances to shoot off spores
foffspr_x <- rep(fparent_xy$x, each = f_spores) + cos(fangle) * fdist
foffspr_y <- rep(fparent_xy$y, each = f_spores) + sin(fangle) * fdist

foffspr_xy <- data.frame(foffspr_x=foffspr_x, foffspr_y=foffspr_y) # landing locations of spores

# match up spores with seedlings/trees (within f_movedist) for next gen infection

names(offspr_pos) <- c("x", "y")
names(parent_xy) <- c("x", "y")
treeloc <- rbind(parent_xy, offspr_pos) # all tree locations

names(fparent_xy) <- c("x", "y")
names(foffspr_xy) <- c("x", "y")
fungiloc <- rbind(fparent_xy, foffspr_xy) # all fungi locations

plot(parent_xy, col="black")
points(offspr_pos, col="green")
points(fparent_xy, col="red")
points(foffspr_xy, col="orange")


pythagdist <- function(x1, x2, y1, y2){
    xdist2 = (x1-x2)^2
    ydist2 = (y1-y2)^2
    zdist = sqrt(xdist2+ydist2)
    return(zdist)
}

#testlist <- pythagdist(treeloc$x, fungiloc$x, treeloc$y, fungiloc$y)

for (i in treeloc$x){
    for (j in fungiloc$x){
        if (pythagdist(treeloc$x[i], fungiloc$x[j], treeloc$y[i], fungiloc$y[j]) < f_movedist){
            return(treeloc$x[i])
            return(treeloc$y[i])
        }
    }
}
# reset fungus to infected trees

# kill off some of the infected trees based on age (95% mortality when young, 5% when old...?)



#
# We would need to add a separate system for the fungi and an interaction so they would be antagonistic