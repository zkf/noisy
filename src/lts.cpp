#include "lts.h"

#include <boost/math/distributions/normal.hpp>
#include <valarray>
#include <iostream>

Lts::Lts(int numArms, double initMean, double initSd,
        double observationNoise)
: _observationNoise(observationNoise)
, _numArms(numArms)
{
    std::cout << _numArms << std::endl;
    //_numArms = 2;
    for(int i = 0; i < numArms; ++i)
    {
        arms[i] = boost::make_tuple(initMean, initSd);
    }
}

int Lts::selectArm()
{
//    std::cout << "_ob: " << _observationNoise << "\n";
//    std::cout << "_numArms: " << _numArms << "\n";
    std::valarray<double> results(_numArms);
    for(size_t i = 0; i < _numArms; ++i)
    {
        results[i] = rndFromNormalDist(arms[i]);
        std::cout << "got rnd: " << results[i] << std::endl;
    }
    return results.max();
    
}

void Lts::update(double reward, int armIndex)
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
