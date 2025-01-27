#ifndef _CO2_EMISSIONS_H_
#define _CO2_EMISSIONS_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
 * \file co2_emissions.h
 * \ingroup Objects
 * \brief CO2Emissions class header file.
 * \author Jim Naslund
 */

#include "emissions/include/aghg.h"

/*! 
 * \ingroup Objects
 * \brief A class that represents CO2 emissions.
 * \author Jim Naslund
 */
class CO2Emissions: public AGHG { 
public:
    CO2Emissions();

    virtual ~CO2Emissions();

    virtual CO2Emissions* clone() const;
    
    virtual void copyGHGParameters( const AGHG* aPrevGHG );
    
    static const std::string& getXMLNameStatic();
    
    virtual const std::string& getXMLName() const;

    virtual double getGHGValue( const std::string& aRegionName,
                                const std::vector<IInput*>& aInputs,
                                const std::vector<IOutput*>& aOutputs,
                                const ICaptureComponent* aSequestrationDevice,
                                const int aPeriod ) const;

	virtual void calcEmission( const std::string& aRegionName, 
                               const std::vector<IInput*>& aInputs,
                               const std::vector<IOutput*>& aOutputs,
					           ICaptureComponent* aSequestrationDevice,
                               const int aPeriod );

protected:
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        AGHG
    )

    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;

private:
    double calcOutputCoef( const std::vector<IOutput*>& aOutputs, const int aPeriod ) const;
    double calcInputCoef( const std::vector<IInput*>& aInputs, const int aPeriod ) const;
    double calcOutputEmissions( const std::vector<IOutput*>& aOutputs,
                                const int aPeriod ) const;
    double calcInputCO2Emissions( const std::vector<IInput*>& aInputs, const std::string& aRegionName,
                                  const int aPeriod ) const;
};

#endif // _CO2_EMISSIONS_H_
