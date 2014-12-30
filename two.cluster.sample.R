########################################################################
## Description: Function to implement a two stage cluster sample in R
##
## Maintainer: Rodrigo Sant'Ana
## Author: Rodrigo Sant'Ana
## Created: Ter Dez 30 21:30:37 2014 (-0200)
## Version: 0.0.1
## Last-Updated: Ter Dez 30 21:31:38 2014 (-0200)
##           By: Rodrigo Sant'Ana
##
## Database info: No data info
##
### Commentary:
##
### Code:
########################################################################

two.cluster.sample <- function(M, m, N, n, prob1 = NULL, prob2 = NULL,
			       replace1 = FALSE, replace2 = FALSE){
    ## Two Stage Cluster Sample function...
    ## M is a vector with references of the first stage clusters to be
    ## sampled...
    ## m is the desired sample size in a first stage cluster...
    ## N is a vector with references of the second stage to be sampled..
    ## n is the desired sample size in a second stage of this sampling..
    ## Probs1 is a vector of probabilities for the first stage of the
    ## sampling process...
    ## Probs2 is a vector of probabilities for the second stage of the
    ## sampling process...
    pu.samp <- data.frame(var1 = sample(unique(M), m, prob = prob1,
					replace = replace1))
    samp <- data.frame(pu = NULL, su = NULL)
    for(i in unique(pu.samp$var1)){
        x <- sample(N[M == i], n, prob = prob2, replace = replace2)
        samp <- rbind(samp, data.frame(pu = i, su = x))
    }
    return(samp)
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
