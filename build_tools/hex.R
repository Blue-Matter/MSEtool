# https://github.com/GuangchuangYu/hexSticker
library(hexSticker)
library(showtext)

font_add_google("Oswald", "Oswald")

# ---- Version 1 -----

# ---- OpenMSE ----
imgurl <- "build_tools/images/img/fish_logo2.png"
outfile <- "build_tools/images/logo/V1/OpenMSE.png"

s <- sticker(imgurl, 
             package='OpenMSE', # package name
             p_size=20,
             p_y=1.4,
             p_family='Oswald',
             p_color='white',
             s_x=1,  # subplot
             s_y=.75, 
             s_width=.4,
             s_height=1,
             spotlight = TRUE,
             l_x=1,
             l_y=.75,
             h_color='#0f2043', # hex color
             h_fill='#355495',
             filename=outfile)

# ---- MSEtool----
imgurl <- "build_tools/images/img/fish_logo2.png"
outfile <- "build_tools/images/logo/V1/MSEtool.png"

s <- sticker(imgurl, 
             package='MSEtool', # package name
             p_size=20,
             p_y=1.4,
             p_family='Oswald',
             p_color='white',
             s_x=1,  # subplot
             s_y=.75, 
             s_width=.4,
             s_height=1,
             spotlight = TRUE,
             l_x=1,
             l_y=.75,
             h_color='#0f2043', # hex color
             h_fill='#355495',
             filename=outfile)

# ---- DLMtool ----
imgurl <- "build_tools/images/img/fish_logo2.png"
outfile <- "build_tools/images/logo/V1/DLMtool.png"

s <- sticker(imgurl, 
             package='DLMtool', # package name
             p_size=20,
             p_y=1.4,
             p_family='Oswald',
             p_color='white',
             s_x=1,  # subplot
             s_y=.75, 
             s_width=.4,
             s_height=1,
             spotlight = TRUE,
             l_x=1,
             l_y=.75,
             h_color='#0f2043', # hex color
             h_fill='#355495',
             filename=outfile)


# ---- MSEtool ----
imgurl <- "build_tools/images/img/fish_logo2.png"
outfile <- "build_tools/images/logo/V1/MSEtool.png"

s <- sticker(imgurl, 
             package='MSEtool', # package name
             p_size=20,
             p_y=1.4,
             p_family='Oswald',
             p_color='white',
             s_x=1,  # subplot
             s_y=.75, 
             s_width=.4,
             s_height=1,
             spotlight = TRUE,
             l_x=1,
             l_y=.75,
             h_color='#0f2043', # hex color
             h_fill='#355495',
             filename=outfile)



# ---- Version 2 -----

# ---- MSEtool ----
imgurl <- "build_tools/images/img/fish5.png"
outfile <- "build_tools/images/logo/V2/MSEtool.png"

s <- sticker(imgurl, 
             package='MSEtool', # package name
             p_size=20,
             p_y=1.4,
             p_family='Oswald',
             p_color='white',
             s_x=1,  # subplot
             s_y=.8, 
             s_width=1,
             s_height=1,
             spotlight = TRUE,
             l_x=1.7,
             l_y=1.3,
             h_color='#0f2043', # hex color
             h_fill='#355495',
             filename=outfile)


# ---- DLMtool ----
imgurl <- "build_tools/images/img/fish1.png"
outfile <- "build_tools/images/logo/V2/DLMtool.png"

s <- sticker(imgurl, 
             package='DLMtool', # package name
             p_size=20,
             p_y=1.4,
             p_family='Oswald',
             p_color='white',
             s_x=1,  # subplot
             s_y=.75, 
             s_width=1.2,
             s_height=1,
             spotlight = TRUE,
             l_x=1.7,
             l_y=1.3,
             h_color='#0f2043', # hex color
             h_fill='#355495',
             filename=outfile)


# ---- SAMtool ----
imgurl <- "build_tools/images/img/fish2.png"
outfile <- "build_tools/images/logo/V2/SAMtool.png"

s <- sticker(imgurl, 
             package='SAMtool', # package name
             p_size=20,
             p_y=1.4,
             p_family='Oswald',
             p_color='white',
             s_x=1,  # subplot
             s_y=.75, 
             s_width=1,
             s_height=1,
             spotlight = TRUE,
             l_x=1.7,
             l_y=1.3,
             l_height=5,
             l_width = 5,
             h_color='#0f2043', # hex color
             h_fill='#355495',
             filename=outfile)




# MSEtool - Version 1

imgurl <- "build_tools/images/fish_white.png"
outfile <- "build_tools/images/MSEtool.png"

s <- sticker(imgurl, 
             package='MSEtool', # package name
             p_size=20,
             p_y=1.4,
             p_family='Oswald',
             p_color='white',
             s_x=1,  # subplot
             s_y=.75, 
             s_width=.8,
             s_height=1,
             spotlight = TRUE,
             l_x=1.7,
             l_y=1.4,
             h_color='#0f2043', # hex color
             h_fill='#355495',
             filename=outfile)

# MSEtool - Version 2 



# colors:
# fish color: #42b4e4
# beige: #d5a458
# dark blue: #0f2043



s <- sticker(~plot(cars, cex=.5, cex.axis=.5, mgp=c(0,.3,0), xlab="", ylab=""),
             package="hexSticker", p_size=20, s_x=.8, s_y=.6, s_width=1.4, s_height=1.2,
             filename="images/baseplot.png")


