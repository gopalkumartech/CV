library(opencv)  #https://github.com/ropensci/opencv
ocv_video(ocv_face)

ocv_video(ocv_edges)

# get webcam size
test <- ocv_picture()
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
})
