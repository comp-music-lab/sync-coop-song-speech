import pymc as pm
import pytensor.tensor as pt
import numpy as np
import arviz as az
import datetime
from scipy.stats import norm

# Setup
rng = np.random.default_rng()
N = rng.integers(low=32, high=128)
x = rng.normal(loc=-5.342, scale=4.932, size=(N, 1)).flatten()
mu_0 = 0
sgm_0 = 10
nu_0 = 2
K = 10
al = 0.25
be = np.linspace(0, 1, K + 1)**(1/al)
M = 1024
J = 4


def logPowNormal(x, mu, sigma, beta):
    return beta*(-0.5*pm.math.log(2*np.pi) - pm.math.log(sigma) - 0.5*(x - mu)**2/sigma**2)


def main(beta):
    with pm.Model():
        p_mu = pm.Normal("mu", mu=mu_0, sigma=sgm_0)
        p_sgm = pm.HalfStudentT('sigma', nu=nu_0)
        pm.Potential("likelihood", logPowNormal(x, p_mu, p_sgm, beta))
        idata = pm.sample(draws=M, chains=J)
        return idata


if __name__ == "__main__":
    x_pt = pt.scalar("x")
    mu_pt = pt.scalar("mu")
    sgm_pt = pt.scalar("sgm")
    be_pt = pt.scalar("be")
    f = logPowNormal(x_pt, mu_pt, sgm_pt, be_pt)

    lnr = np.zeros([K, 1])

    for k in range(0, K):
        print("K = {}, {}".format(k, datetime.datetime.now()))
        idata = main(be[k])
        mu_pos = idata.posterior["mu"].to_numpy().flatten()
        sgm_pos = idata.posterior["sigma"].to_numpy().flatten()
        #az.plot_trace(idata)

        l_theta = np.zeros([M * J, 1])
        dbe =be[k + 1] - be[k]
        for m in range(0, M*J):
            for i in range(0, N):
                l_theta[m] = l_theta[m] + f.eval({x_pt: x[i], mu_pt: mu_pos[m], sgm_pt: sgm_pos[m], be_pt: dbe})
        lnr[k] = np.max(l_theta) + np.log(np.sum(np.exp(l_theta - np.max(l_theta)))) - np.log(M*J)

    lnZ = np.sum(lnr)
    print("lnZ (Stepping-stone sampling): {}".format(lnZ))

    # test
    lnZ_theta = np.zeros((M*J, 1))
    for m in range(0, M*J):
        mu_m = rng.normal(loc=mu_0, scale=sgm_0)
        sgm_m = np.abs(rng.standard_t(nu_0))
        lnZ_theta[m] = np.sum(norm.logpdf(x, mu_m, sgm_m))
    lnZ_test = np.max(lnZ_theta) + np.log(np.sum(np.exp(lnZ_theta - np.max(lnZ_theta)))) - np.log(M*J)
    print("lnZ (Simple Monte Carlo): {}".format(lnZ_test))

    print("Completed")
