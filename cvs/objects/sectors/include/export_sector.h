#ifndef _EXPORT_SECTOR_H_
#define _EXPORT_SECTOR_H_
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
* \file export_sector.h
* \ingroup Objects
* \brief The ExportSector class header file.
* \author Josh Lurz
*/
#include <string>
#include "sectors/include/supply_sector.h"
class IInfo;

/*! 
* \ingroup Objects
* \brief A SupplySector which can export a good to other regions.
* \details The ExportSector currently is a SupplySector with all fixed output
*          and a read-in international market. It also reads-in prices which are
*          used as the market prices for the good. It creates a non-regional
*          market for the ExportGood, and ensures that simultaneities are not
*          created for it. The ExportSector does not reset the market price.
* \todo Improve this class so more dynamic behavior is possible.
* \author Josh Lurz
*/
class ExportSector: public SupplySector
{
public:
	explicit ExportSector( const std::string& aRegionName );

	virtual void calcFinalSupplyPrice( const GDP* aGDP,
                                       const int aPeriod );

    virtual void supply( const GDP* aGDP,
                         const int aPeriod );

    static const std::string& getXMLNameStatic();
protected:
	void setMarket();
        
	virtual double getPrice( const GDP* aGDP,
                             const int aPeriod ) const;

    virtual bool XMLDerivedClassParse( const std::string& aNodeName,
                                       const xercesc::DOMNode* aCurr ); 
    
    virtual void toInputXMLDerived( std::ostream& aOut,
                                    Tabs* aTabs ) const;
    
    virtual void toDebugXMLDerived( const int aPeriod,
                                    std::ostream& aOut,
                                    Tabs* aTabs ) const;
	
    const std::string& getXMLName() const;
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        SupplySector,
    
        //! Vector of read-in fixed prices.
        DEFINE_VARIABLE( ARRAY, "sectorprice", mFixedPrices, objects::PeriodVector<double> ),

        //! The market region into which the sector is exporting.
        DEFINE_VARIABLE( SIMPLE, "market", mMarketName, std::string )
    )
};

#endif // _EXPORT_SECTOR_H_
