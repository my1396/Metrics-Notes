## pdynmc
library(pdynmc)
data("EmplUK", package = "plm")

EmplUK_log <- EmplUK
EmplUK_log[,c(4:7)] <- log(EmplUK_log[,c(4:7)])
head(EmplUK_log)

## data structure
data.info(EmplUK_log, i.name = "firm", t.name = "year")
strucUPD.plot(EmplUK_log, i.name = "firm", t.name = "year")

# Run pdynmc
# No time effects --------------------------------------------------------------
m0 <- pdynmc(
    dat = EmplUK_log, varname.i = "firm", varname.t = "year",
    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
    include.dum = FALSE,
    w.mat = "iid.err", std.err = "corrected",
    estimation = "onestep", opt.meth = "none" 
)

summary(m0)
mtest.fct(m0)
jtest.fct(m0)
wald.fct(m0, param = "all")

# time effects, one-step -------------------------------------------------------
m1 <- pdynmc(
    dat = EmplUK_log, varname.i = "firm", varname.t = "year",
    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
    w.mat = "iid.err", std.err = "corrected",
    estimation = "onestep", opt.meth = "none" 
)

summary(m1)
mtest.fct(m1)
jtest.fct(m1)
wald.fct(m1, param = "all")

# time effects, two-step -------------------------------------------------------
m2 <- pdynmc(
    dat = EmplUK_log, varname.i = "firm", varname.t = "year",
    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
    varname.reg.fur = c("wage", "capital", "output"), lagTerms.reg.fur = c(1,2,2),
    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = "year",
    w.mat = "iid.err", std.err = "corrected", 
    estimation = "twostep", opt.meth = "none"
)


summary(m2)
mtest.fct(m2)
jtest.fct(m2)
wald.fct(m2, param = "all")

# ============================================================================ #
# linear time trends -----------------------------------------------------------
EmplUK_log_panel <- EmplUK_log %>% 
    pdata.frame(index=c("firm", "year")) %>% 
    make.pbalanced()
N.obs <- EmplUK_log_panel$firm %>% n_distinct()
T.obs <- EmplUK_log_panel$year %>% n_distinct()
cat (sprintf("No.sample: %s \nNo.time: %s \n", N.obs, T.obs) )

## create country specific trend variables: `regressor_t`
## kronecker product %x%
ttrend <- diag(N.obs) %x% matrix(1:T.obs, ncol=1)
colnames(ttrend) <- paste("T1", 1:N.obs, sep="_")

colnames(ttrend)

ttrend %>% dim()
EmplUK_log_panel %>% dim()
EmplUK_log_panel <- cbind(EmplUK_log_panel, ttrend)

m3 <- pdynmc(
    dat = EmplUK_log_panel, varname.i = "firm", varname.t = "year",
    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
    varname.reg.fur = c("wage", "capital", "output", colnames(ttrend)), 
    lagTerms.reg.fur = c(1,2,2, rep(0, N.obs)),
    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = c("year"),
    w.mat = "iid.err", std.err = "corrected", 
    estimation = "twostep", opt.meth = "none"
)

summary(m3)
mtest.fct(m3)
jtest.fct(m3)
wald.fct(m3, param = "all")

# ============================================================================ #
# quadratic time trends --------------------------------------------------------

ttrend2 <- diag(N.obs) %x% matrix((1:T.obs)^2, ncol=1)
colnames(ttrend2) <- paste("T2", 1:N.obs, sep="_")
EmplUK_log_panel <- cbind(EmplUK_log_panel, ttrend2)

m4 <- pdynmc(
    dat = EmplUK_log_panel, varname.i = "firm", varname.t = "year",
    use.mc.diff = TRUE, use.mc.lev = FALSE, use.mc.nonlin = FALSE,
    include.y = TRUE, varname.y = "emp", lagTerms.y = 2,
    fur.con = TRUE, fur.con.diff = TRUE, fur.con.lev = FALSE,
    varname.reg.fur = c("wage", "capital", "output", 
                        colnames(ttrend), colnames(ttrend2)), 
    lagTerms.reg.fur = c(1,2,2, rep(0, N.obs*2)),
    include.dum = TRUE, dum.diff = TRUE, dum.lev = FALSE, varname.dum = c("year"),
    w.mat = "iid.err", std.err = "corrected", 
    estimation = "twostep", opt.meth = "none"
)

summary(m4)
mtest.fct(m4)
jtest.fct(m4)
wald.fct(m4, param = "all")



