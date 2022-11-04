#install.packages("opencv")
library("opencv")
library("psych")

unconf <- ocv_read('https://jeroen.github.io/images/unconf18.jpg')
faces <- ocv_face(unconf)
faces

ocv_write(faces, 'faces.jpg')

#Or get the face location data:
facemask <- ocv_facemask(unconf)
attr(facemask, 'faces')


library(opencv)
ocv_video(ocv_face)


library(opencv)
library(ggplot2)

# get webcam size
test <- ocv_picture()

bitmap <- ocv_bitmap(test)
width <- dim(bitmap)[2]
height <- dim(bitmap)[3]

png('bg.png', width = width, height = height)
par(ask=FALSE)
print(ggplot2::qplot(speed, dist, data = cars, geom = c("smooth", "point")))
dev.off()
bg <- ocv_read('bg.png')
unlink('pg.png')
ocv_video(function(input){
  mask <- ocv_mog2(input)
  return(ocv_copyto(input, bg, mask))
})


#Put your face in the plot:

# Overlay face filter
ocv_video(function(input){
  mask <- ocv_facemask(input)
  ocv_copyto(input, bg, mask)
})



# Live Face Survey
#Go stand on the left if you’re a tidier

library(opencv)

# get webcam size
test <- ocv_picture()
test
bitmap <- ocv_bitmap(test)
width <- dim(bitmap)[2]
height <- dim(bitmap)[3]

# generates the plot
makeplot <- function(x){
  png('bg.png', width = width, height = height, res = 96)
  on.exit(unlink('bg.png'))
  groups <- seq(0, width, length.out = 4)
  left <- rep("left", sum(x < groups[2]))
  middle <- rep("middle", sum(x >= groups[2] & x < groups[3]))
  right <- rep("right", sum(x >= groups[3]))
  f <- factor(c(left, middle, right), levels = c('left', 'middle', 'right'),
              labels = c("Tidy!", "Whatever Works", "Base!"))
  color = I(c("#F1BB7B", "#FD6467", "#5B1A18"))
  plot(f, ylim = c(0, 5),
       main = "Are you a tidyer or baser?", col = color)
  dev.off()
  ocv_read('bg.png')
}

# overlays faces on the plot
ocv_video(function(input){
                          mask <- ocv_facemask(input)
                          faces <- attr(mask, 'faces')
                          bg <- makeplot(faces$x)
                         return(ocv_copyto(input, bg, mask))
                       }      )


# fce on plot
# Overlay face filter
ocv_video(function(input){
                            mask <- ocv_facemask(input)
                           ocv_copyto(input, bg, mask)
                        } ) 


# Silly example
mona <- ocv_read('https://jeroen.github.io/images/monalisa.jpg')
mona

mona <- ocv_read('/home/pedqam/Desktop/gkgit/pedqam/gkgoa2019.jpg')



ocv_edges(mona)  # Edge detection
ocv_markers(mona)

# Find face
faces <- ocv_face(mona)
faces

# To show locations of faces
facemask <- ocv_facemask(mona)
attr(facemask, 'faces')
facemask


ocv_destroy(mona)  # This is not strictly needed

##############
ocv_version()

image = '/home/pedqam/Desktop/gkgit/pedqam/gkgoa2019.jpg'
unconf <- ocv_read(image)

faces <- ocv_face(unconf)



ocv_display(image)
ocv_grayscale(image)
ocv_picture()  # it give snap from webcam
# ocv_video() # error

## from webcam
test <- ocv_picture()
test

ocv_video(filter)

##### 
ocv_video(ocv_face)
ocv_video(ocv_edges)
ocv_video(ocv_knn)
ocv_video(ocv_facemask)




##
test <- opencv::ocv_camera()

