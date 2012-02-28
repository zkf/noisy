#include <iostream>
#include "lts.h"
#include <boost/tuple/tuple.hpp>
#include <boost/random.hpp>

using namespace boost;
typedef tuple<double, double> arm;

double rndFromNormalDist(arm& a)
{
    double mean = boost::get<0>(a);
    double sd   = boost::get<1>(a);
    static random::mt19937 rng(static_cast<unsigned> (std::time(0))); 
    boost::random::normal_distribution<double> norm_dist(mean, sd);
    boost::random::variate_generator<boost::random::mt19937&,
        boost::random::normal_distribution<double> > normal_sampler(rng, norm_dist);
    return normal_sampler();
}

double observationNoise = 5.0;
double initMean = 3.5;
double initSd   = 3.0;

int T = 10;


arm arm1 = make_tuple(5.0, 2.0);
arm arm2 = make_tuple(2.0, 2.0);

arm arms[] = {arm1, arm2};

double cumulativeReward = 0.0;

int main(int argc, char* argv[])
{
    Lts bandit( 3, initMean, initSd, observationNoise );
    std::cout << "Starting simulation" << "\n";

    for(int i = 0; i <= T; ++i)
    {
        int selectedArm = bandit.selectArm();
        std::cout << "Selected arm " << selectedArm << "\n";
        double reward = rndFromNormalDist(arms[selectedArm]);
        cumulativeReward += reward;
        bandit.update(reward, selectedArm);
    }
    
    std::cout << "Done with the simulation.\n";
    std::cout << "Cumulative reward: " << cumulativeReward
        << " after " << T << " rounds using ob: " << observationNoise
        << std::endl;
    return 0;
}
