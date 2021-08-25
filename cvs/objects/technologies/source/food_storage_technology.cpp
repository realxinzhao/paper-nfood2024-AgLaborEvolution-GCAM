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
 * \file pass_through_technology.cpp
 * \ingroup Objects
 * \brief FoodStorageTechnology class source file.
 * \author Ellie Lochner
 */

// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include "technologies/include/food_storage_technology.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/market_dependency_finder.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/production_state_factory.h"


using namespace std;
using namespace xercesc;

extern Scenario* scenario;

FoodStorageTechnology::FoodStorageTechnology( const string& aName, const int aYear ):
Technology(aName, aYear )
{
}

FoodStorageTechnology::~FoodStorageTechnology() {
}

FoodStorageTechnology* FoodStorageTechnology::clone() const {
    FoodStorageTechnology* clone = new FoodStorageTechnology( mName, mYear );
    clone->copy( *this );
    return clone;
}

void FoodStorageTechnology::copy( const FoodStorageTechnology& aOther ) {
    Technology::copy( aOther );
    
}

const string& FoodStorageTechnology::getXMLNameStatic() {
    const static string XML_NAME = "food-storage-technology";

    return XML_NAME;
}

const string& FoodStorageTechnology::getXMLName() const {
    return getXMLNameStatic();
}

void FoodStorageTechnology::completeInit(const string& aRegionName,
    const string& aSectorName,
    const string& aSubsectorName,
    const IInfo* aSubsectorInfo,
    ILandAllocator* aLandAllocator)
{

    Technology::completeInit(aRegionName, aSectorName, aSubsectorName, aSubsectorInfo, aLandAllocator);

    // The FoodStorageTechnology should not have any vintaging.  All vintaging should be
    // in the associated pass-through sector
    // check if lifetime years covers 2 periods
    const Modeltime* modeltime = scenario->getModeltime();
    
    if (mYear < modeltime->getEndYear()) {
        const int period = modeltime->getyr_to_per(mYear);
        int expectedLifetime = modeltime->gettimestep(period) + modeltime->gettimestep(period + 1);
    }
}

void FoodStorageTechnology::setProductionState(const int aPeriod) {
    // Check that the state for this period has not already been initialized.
    // Note that this is the case when the same scenario is run multiple times
    // for instance when doing the policy cost calculation.  In which case
    // we must delete the memory to avoid a memory leak.
    if (mProductionState[aPeriod]) {
        delete mProductionState[aPeriod];
    }

    double initialOutput = 0;
    const Modeltime* modeltime = scenario->getModeltime();

    if (aPeriod <= modeltime->getFinalCalibrationPeriod()) {
        initialOutput = mCarriedForwardValue;
    }
    else {
        initialOutput = mStoredValue * 0.75; //lose 25%
    }

    mProductionState[aPeriod] =
        ProductionStateFactory::create(mYear, mLifetimeYears, mFixedOutput,
            initialOutput, aPeriod).release();
}


void FoodStorageTechnology::initCalc(const string& aRegionName,
    const string& aSectorName,
    const IInfo* aSubsectorInfo,
    const Demographic* aDemographics,
    PreviousPeriodInfo& aPrevPeriodInfo,
    const int aPeriod) {

    Technology::initCalc(aRegionName, aSectorName, aSubsectorInfo, aDemographics, aPrevPeriodInfo, aPeriod);

    if (aPeriod > 0) {
        mExpectedPrice = scenario->getMarketplace()->getPrice(aSectorName, aRegionName, aPeriod-1);
    }
    else {
        mExpectedPrice = 1;
    }
}

void FoodStorageTechnology::production(const string& aRegionName,
    const string& aSectorName,
    double aVariableDemand,
    double aFixedOutputScaleFactor,
    const GDP* aGDP,
    const int aPeriod)
{
    if (mProductionState[aPeriod]->isNewInvestment()) {

        const Modeltime* modeltime = scenario->getModeltime();

        if (aPeriod <= modeltime->getFinalCalibrationPeriod()) {
            mShareWeight = mInitialStock / ((pow(mExpectedPrice / mInputs[0]->getPrice(aRegionName, aPeriod), mLogitExponent)) * aVariableDemand);
        }

        mStoredValue = mShareWeight * (pow(mExpectedPrice / mInputs[0]->getPrice(aRegionName, aPeriod), mLogitExponent))*aVariableDemand;
        double total = mStoredValue + aVariableDemand;
        double totalToVariableRatio = total / aVariableDemand;
        mInputs[0]->setCoefficient(totalToVariableRatio, aPeriod);
    }
    else if(mProductionState[aPeriod]->isOperating()){
        mInputs[0]->setCoefficient(0, aPeriod);
    }
    Technology::production(aRegionName, aSectorName, aVariableDemand, aFixedOutputScaleFactor, aGDP, aPeriod);
}



bool FoodStorageTechnology::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aNode ) {
    if (aNodeName == "carried-forward") { //what you get from previous period
        mCarriedForwardValue = XMLHelper<Value>::getValue(aNode);
        return true;
    }
    else if (aNodeName == "initial-stock") { //calibrated value from that period
        mInitialStock = XMLHelper<Value>::getValue(aNode);
        return true;
    }
    else if (aNodeName == "logit-exponent") {
        mLogitExponent = XMLHelper<Value>::getValue(aNode);
        return true;
    }
    return false;
}

//! write object to xml output stream
void FoodStorageTechnology::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement(mStoredValue, "stored-value", aOut, aTabs);
    XMLWriteElement(mExpectedPrice, "expected-price", aOut, aTabs);
}

