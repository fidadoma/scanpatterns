.onLoad <- function(libname, pkgname) {
  assign("pkg_globals", new.env(), envir=parent.env(environment()))
  assign("eye-dir", "cache/eye", pkg_globals)
  assign("spaces-dir", "cache/space", pkg_globals)
  assign("arenamin", -15, pkg_globals)
  assign("arenamax", 15, pkg_globals)
  assign("xystep", 0.25, pkg_globals)
  assign("tstep", 0.02, pkg_globals)
  assign("tmax", 6, pkg_globals)
  assign("max-time", 5997, pkg_globals)
  assign("min-time", 1, pkg_globals)
  assign("step-time", 4, pkg_globals)
  assign("xsd", 1.2, pkg_globals)
  assign("ysd", 1.2, pkg_globals)
  assign("tsd", 0.0265, pkg_globals)
  
}
.onAttach <- function(libname, pkgname) {
  
  assign("eye-dir", "cache/eye", pkg_globals)
  assign("spaces-dir", "cache/space", pkg_globals)
  assign("arenamin", -15, pkg_globals)
  assign("arenamax", 15, pkg_globals)
  assign("xystep", 0.25, pkg_globals)
  assign("tstep", 0.02, pkg_globals)
  assign("tmax", 6, pkg_globals)
  assign("max-time", 5997, pkg_globals)
  assign("min-time", 1, pkg_globals)
  assign("step-time", 4, pkg_globals)
  assign("xsd", 1.2, pkg_globals)
  assign("ysd", 1.2, pkg_globals)
  assign("tsd", 0.0265, pkg_globals)
  
}

scanpatterns.set.parameters<-function(l) {
  for(i in 1:length(l)) {
    assign(names(l)[i], l[[i]], pkg_globals)
  }
}
