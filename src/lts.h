#include <boost/tuple/tuple.hpp>
#include <boost/random.hpp>

class Lts 
{
    public:
        Lts(int numArms, double initMean, double initSd,
                double observationNoise);
        /*
         * selectArm - randomly select an arm.
         * @return the arm index.
         */
        int selectArm();
        
        /*
         * update - update distribution estimates for arm based on @reward.
         */
        void update(double reward);
        
    private:
        typedef boost::tuple<double, double> normalDist;
        
        // an arm is (mean, variance).
        normalDist arms[];

        double observationNoise;
        
        int numArms;
        
        // index of last selected arm.
        unsigned int lastArm;
        
        double rndFromNormalDist(normalDist& nd);
    
};
