########################################################################
## Description: Generalized Linear Model diagnostic plot with GGPLOT2
##
## Maintainer: Rodrigo Sant'Ana
## Author: Rodrigo Sant'Ana
## Created: Seg Dez 29 21:36:21 2014 (-0200)
## Version: 0.0.1
## Last-Updated: Ter Dez 30 21:23:36 2014 (-0200)
##           By: Rodrigo Sant'Ana
##
## Database info: No data based...
##
### Commentary: This function could be applied to Linear Models and
### Generalized Linear Models only...
##
### Code:
########################################################################

ggplot.diag <- function(x, ..., which = 1:6, mfrow = c(1, 1)) {
    # Pacotes requeridos...
    require(ggplot2)
    require(gridExtra)
    # Atribuindo o resumo do modelo a um objeto... não funciona!!!
    df <- fortify(model)
    df <- cbind(df, rows = 1:nrow(df))
    # Valores ajustados x residuos...
    g1 <- ggplot(na.omit(df), aes(.fitted, .resid)) +
        geom_point(pch = 21, size = 2.5, fill = "white", na.rm = TRUE) +
        stat_smooth(method = "loess", na.rm = TRUE) +
        geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
        labs(x = "Valores ajustados", y = "Residuos") +
        theme(axis.text.x = element_text(size = 12, colour = "black"),
              axis.text.y = element_text(size = 12, colour = "black"))
    # qqnorm...
    g2 <- ggplot(na.omit(df), aes(sample = .stdresid)) +
        stat_qq(pch = 21, size = 2.5, fill = "white", na.rm = TRUE) +
        geom_abline(col = "red") +
        labs(x = "Quantís teóricos", y = "Residuos padronizados") +
        theme(axis.text.x = element_text(size = 12, colour = "black"),
              axis.text.y = element_text(size = 12, colour = "black"))
    # Localizacao-escala...
    g3 <- ggplot(na.omit(df), aes(.fitted, sqrt(abs(.stdresid)))) +
        geom_point(pch = 21, size = 2.5, fill = "white", na.rm = TRUE) +
        stat_smooth(method = "loess", na.rm = TRUE) +
        labs(x ="Valores ajustados",
             y = expression(sqrt("Residuos padronizados"))) +
        theme(axis.text.x = element_text(size = 12, colour = "black"),
              axis.text.y = element_text(size = 12, colour = "black"))
    # Distancia de Cook's...
    g4 <- ggplot(df, aes(rows, .cooksd,
                         ymin = 0, ymax = .cooksd)) +
        geom_point(pch = 21, size = 2.5, fill = "white", na.rm = TRUE) +
        geom_linerange() +
        scale_x_continuous(breaks = seq(0, dim(df)[1], 20)) +
        labs(x = "Número de observações", y = "Distâncias de Cook") +
        theme(axis.text.x = element_text(size = 12, colour = "black"),
              axis.text.y = element_text(size = 12, colour = "black"))
    # Residuos x Alavancagem...
    g5 <- ggplot(df, aes(.hat, .stdresid)) +
        geom_point(aes(size = .cooksd), pch = 21,
                   fill = "white", na.rm = TRUE) +
        stat_smooth(method = "loess", na.rm = TRUE) +
        geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
        scale_size_continuous("Distância de Cook", range = c(2, 5)) +
        labs(x = "Alavancagem", y = "Residuos padronizados") +
        theme(axis.text.x = element_text(size = 12, colour = "black"),
              axis.text.y = element_text(size = 12, colour = "black"))
    # Cook's x Alavancagem...
    g6 <- ggplot(df, aes(.hat, .cooksd)) +
        geom_point(pch = 21, size = 2.5, fill = "white", na.rm = TRUE) +
        stat_smooth(method = "loess", na.rm = TRUE) +
        ## geom_abline(slope = seq(0, max(df$.cooksd, na.rm = TRUE), l = 10),
        ##             color = "gray", linetype = "dashed") +
        ## annotate("text", x = rep(max(df$.hat), 10),
        ##          y = seq(0.002, max(df$.cooksd, na.rm = TRUE)+0.002, l = 10),
        ##          label = round(seq(0, max(df$.cooksd, na.rm = TRUE), l =
        ##              10), 2),
        ##          size = 3, colour = "gray",
        ##          angle = seq(0, 35, l = 10)) +
        labs(x = "Alavancagem", y = "Distância de Cook") +
        theme(axis.text.x = element_text(size = 12, colour = "black"),
              axis.text.y = element_text(size = 12, colour = "black"))
    ## salvando as figuras...
    plots <- list(g1, g2, g3, g4, g5, g6)
    ## plotando somente as selecionadas...
    grid.newpage()
    if (prod(mfrow)>1) {
        mypos <- expand.grid(1:mfrow[1], 1:mfrow[2])
        mypos <- mypos[with(mypos, order(Var1)), ]
        pushViewport(viewport(layout = grid.layout(mfrow[1], mfrow[2])))
        formatter <- function(.){}
    } else {
        mypos <- data.frame(matrix(1, length(which), 2))
        pushViewport(viewport(layout = grid.layout(1, 1)))
        formatter <- function(.) {
            .dontcare <- readline("Pressione <Enter> para ver o próximo gráfico: ")
            grid.newpage()
        }
    }
    j <- 1
    for (i in which){
        formatter()
        print(plots[[i]], vp=viewport(layout.pos.row = mypos[j,][1],
                              layout.pos.col = mypos[j,][2]))
        j <- j+1
    }
}


########################################################################
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING.  If not, write to
## the Free Software Foundation, Inc., 51 Franklin Street, Fifth
## Floor, Boston, MA 02110-1301, USA.
##
########################################################################
