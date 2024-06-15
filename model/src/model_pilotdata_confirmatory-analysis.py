import pymc as pm
import pytensor.tensor as pt
from pytensor.tensor import TensorVariable
from typing import Optional, Tuple
import numpy as np
import scipy as sp
import arviz as az
from datetime import datetime
import pandas as pd


def logp_nonlocal(value:TensorVariable, mu: TensorVariable, Sigma: TensorVariable) -> TensorVariable:
    p = mu.type.shape[0]
    z = value - mu

    '''
    PyTensor produces errors when generating a function graph in this step,
    though this function can run without errors during runtime...
    '''
    Lambda = pm.math.matrix_inverse(Sigma)

    return (2*pm.math.log(pm.math.abs(value[1])) - pm.math.log(Sigma[1,1])
            -(p/2)+pm.math.log(2*np.pi) - 0.5*pm.math.logdet(Sigma) -
            0.5*pt.linalg.matrix_dot(z.T, Lambda, z))


def random_nonlocal(mu: np.ndarray | float, Sigma: np.ndarray | float,
                    rng: Optional[np.random.Generator]=None,
                    size : Optional[Tuple[int]]=None) -> np.ndarray | float:
    p = mu.shape[0]
    g = sp.stats.multivariate_normal(mean=mu, cov=Sigma, seed=rng)
    f = sp.stats.multivariate_t(df=p+1, shape=Sigma, seed=rng)

    Y = f.rvs(size)
    u = np.random.uniform(0, 1)
    r = (Y[1]**2)/Sigma[1,1]*g.pdf(Y) / (4*d*f.pdf(Y))
    while np.all(u >= r):
        Y = f.rvs()
        u = np.random.uniform(0, 1)
        r = (Y[1]**2)/Sigma[1, 1]*g.pdf(Y) / (4*d*f.pdf(Y))

    return Y


def momdist_nonlocal(rv: TensorVariable, size: TensorVariable, mu: TensorVariable, Sigma: TensorVariable) -> TensorVariable:
  return mu + np.sqrt(2)*pt.sqrt(pt.diagonal(Sigma))


def main(y, Z, c, X):
    N, C = Z.shape
    d = X.shape[1]
    iX = np.linalg.pinv(X)

    with pm.Model():
        λ = pm.HalfStudentT.dist(nu=2, sigma=np.repeat(1000, C))
        L, corr, sigmas = pm.LKJCholeskyCov('L', eta=1, n=C, sd_dist=λ)
        D = pm.math.matmul(L, L.T)
        u = pm.MvNormal('u', mu=np.zeros(C), cov=D)
        s = pm.HalfStudentT('s', nu=2, sigma=1000)
        Σ = N * pt.linalg.matrix_dot(iX, pt.linalg.matrix_dot(Z, D, Z.T) + s * np.eye(N), iX.T)
        β = pm.CustomDist('β', np.zeros(d), Σ, logp=logp_nonlocal, random=random_nonlocal, moment=momdist_nonlocal, signature='()->()')
        μ = pm.math.matmul(X, β) + u[c]
        pm.Normal('y', mu=μ, sigma=s, observed=y)

        '''
        The numbers of draws and tune should be larger in the actual analysis.
        draws=512 and tune=500 are used to avoid too long computation and to run as a minimally working example.
        '''
        idata = pm.sample(draws=512, chains=4, tune=500, target_accept=0.95)
        return idata


if __name__ == "__main__":
    # Load pilot data
    filepath = './keydata_long.csv'
    data = pd.read_csv(filepath)
    y = data["score"][20:36]
    N = 16
    X = np.stack([np.ones(N), (data["group"][20:36]=="GS")*1, data["score"][0:N]], axis=1)
    c = np.concatenate(
        [np.repeat(0, 3), np.repeat(1, 4),
         np.repeat(2, 6), np.repeat(3, 3)]
    )
    Z = np.zeros((N, 4))
    for i in range(N):
        Z[i, c[i]] = 1

    # Run simulation
    print(datetime.now())
    idata = main(y, Z, c, X)
    az.plot_trace(idata, filter_vars="regex", var_names=['β', 's', 'u'])
    az.plot_energy(idata)
    az.summary(idata, round_to=2)
    print(np.nanmedian(idata.posterior["β"].to_numpy(), axis=(0, 1)))
    print(np.nanmedian(idata.posterior["u"].to_numpy(), axis=(0, 1)))

    #TODO: return the mode of the posterior distribution insted of the median of the posterior samples
    print(np.nanmedian(idata.posterior["L_stds"].to_numpy(), axis=(0, 1)))

    print('Completed')