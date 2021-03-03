shot_angle <- function(shot_x, shot_y){
  post_x <- 186.7
  post_top_y <- 45.5
  post_bot_y <- 39.5
  if(is.na(shot_x) | shot_x == '' | is.null(shot_x) ){ #No data
    phi <- NA
  }
  else if(shot_x > 188){ #Behind the goal
    phi <- 0
  }
  else if(shot_y > post_top_y | shot_y < post_bot_y){ #Below or above the goal
    adj <- sqrt((shot_x-post_x)^2)
    hip_1 <- sqrt((shot_x-post_x)^2+(shot_y-post_top_y)^2)
    hip_2 <- sqrt((shot_x-post_x)^2+(shot_y-post_bot_y)^2)
    opp_1 <- sqrt((shot_y-post_top_y)^2)
    opp_2 <- sqrt((shot_y-post_bot_y)^2)
    alpha <- asin(opp_2/hip_2)
    beta <- asin(opp_1/hip_1)
    phi <- abs(alpha-beta)
  }
  else if(shot_y == post_top_y | shot_y == post_bot_y){ # At the goalpost
    adj <- sqrt((shot_x-post_x)^2)
    hip <- sqrt((shot_x-post_x)^2+(shot_y-post_bot_y)^2)
    opp <- sqrt((post_top_y-post_bot_y)^2)
    phi <- abs(asin(opp/hip))
  }
  else if(shot_y < post_top_y & shot_y > post_bot_y){ #Between the goalposts
    adj <- sqrt((shot_x-post_x)^2)
    hip_1 <- sqrt((shot_x-post_x)^2+(shot_y-post_top_y)^2)
    hip_2 <- sqrt((shot_x-post_x)^2+(shot_y-post_bot_y)^2)
    opp_1 <- sqrt((shot_y-post_top_y)^2)
    opp_2 <- sqrt((shot_y-post_bot_y)^2)
    alpha <- asin(opp_2/hip_2)
    beta <- asin(opp_1/hip_1)
    phi <- alpha+beta
  }
  return(phi)
}


shot_dist <- function(shot_x, shot_y){
  goal_x <- 186.7
  goal_y <- 42.5
  distance <- sqrt((shot_x-goal_x)^2+(shot_y-goal_y)^2)
  return(distance)
}

shot_dist(187, 40)
shot_angle(192, 86)

stan <- function(variable){
  var_stan <-(variable-mean(variable, na.rm = T))/sd(variable, na.rm = T)
}

shot_angle_ohl <- function(shot_x, shot_y){
  post_x <- 189
  post_top_y <- 45.5
  post_bot_y <- 39.5
  if(is.na(shot_x) | shot_x == '' | is.null(shot_x) ){ #No data
    phi <- NA
  }
  else if(shot_x > 191.5){ #Behind the goal
    phi <- 0
    return(phi)
  }
  else if(shot_y > post_top_y | shot_y < post_bot_y){ #Below or above the goal
    adj <- sqrt((shot_x-post_x)^2)
    hip_1 <- sqrt((shot_x-post_x)^2+(shot_y-post_top_y)^2)
    hip_2 <- sqrt((shot_x-post_x)^2+(shot_y-post_bot_y)^2)
    opp_1 <- sqrt((shot_y-post_top_y)^2)
    opp_2 <- sqrt((shot_y-post_bot_y)^2)
    alpha <- asin(opp_2/hip_2)
    beta <- asin(opp_1/hip_1)
    phi <- abs(alpha-beta)
    return(phi)
  }
  else if(shot_y == post_top_y | shot_y == post_bot_y){ # At the goalpost
    adj <- sqrt((shot_x-post_x)^2)
    hip <- sqrt((shot_x-post_x)^2+(shot_y-post_bot_y)^2)
    opp <- sqrt((post_top_y-post_bot_y)^2)
    phi <- abs(asin(opp/hip))
    return(phi)
  }
  else if(shot_y < post_top_y & shot_y > post_bot_y){ #Between the goalposts
    adj <- sqrt((shot_x-post_x)^2)
    hip_1 <- sqrt((shot_x-post_x)^2+(shot_y-post_top_y)^2)
    hip_2 <- sqrt((shot_x-post_x)^2+(shot_y-post_bot_y)^2)
    opp_1 <- sqrt((shot_y-post_top_y)^2)
    opp_2 <- sqrt((shot_y-post_bot_y)^2)
    alpha <- asin(opp_2/hip_2)
    beta <- asin(opp_1/hip_1)
    phi <- alpha+beta
    return(phi)
  }
  return(phi)
}


shot_dist_ohl <- function(shot_x, shot_y){
  goal_x <- 189
  goal_y <- 42.5
  distance <- sqrt((shot_x-goal_x)^2+(shot_y-goal_y)^2)
  return(distance)
}

perc.rank <- function(x) trunc(rank(x))/length(x)
