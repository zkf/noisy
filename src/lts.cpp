#include "lts.h"

#include <boost/math/distributions/normal.hpp>
#include <valarray>

Lts::Lts(int numArms, double initMean, double initSd,
        double observationNoise)
: observationNoise(observationNoise)
{
    for(int i = 0; i <= numArms; ++i)
    {
        arms[i] = boost::make_tuple(initMean, initSd);
    }
    
    lastArm = -1;
}

int Lts::selectArm()
{
    std::valarray<double> results(numArms);
    for(size_t i = 0; i <= numArms; ++i)
    {
        results[i] = rndFromNormalDist(arms[i]);
    }
    lastArm = results.max();
    return lastArm;
    
}

void Lts::update(double reward)
{
    
}


double Lts::rndFromNormalDist(normalDist& nd)
{
    double mean = boost::get<0>(nd);
    double sd   = boost::get<1>(nd);
    static boost::random::mt19937 rng(static_cast<unsigned> (std::time(0))); 
    boost::random::normal_distribution<double> norm_dist(mean, sd);
    boost::random::variate_generator<boost::random::mt19937&,
        boost::random::normal_distribution<double> > normal_sampler(rng, norm_dist);
    return normal_sampler();
}
