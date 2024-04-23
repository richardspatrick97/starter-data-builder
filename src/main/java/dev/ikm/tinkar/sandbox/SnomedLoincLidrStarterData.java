package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.id.PublicId;
import dev.ikm.tinkar.common.id.PublicIds;
import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.io.File;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.logging.Logger;
public class SnomedLoincLidrStarterData {
    private static final Logger LOG = Logger.getLogger(SnomedLoincLidrStarterData.class.getSimpleName());

    private static File exportDataStore;
    private static File exportFile;

    private static HashMap <String, EntityProxy.Concept> fullyQualifiedNameToConceptMap = new HashMap<>();
    public static void main(String args[]){

        exportDataStore = new File(args[0]);
        exportFile = new File(args[1]);

        UUIDUtility uuidUtility = new UUIDUtility();

        StarterData starterData = new StarterData(exportDataStore, uuidUtility)
                .init()
                .authoringSTAMP(
                        TinkarTerm.ACTIVE_STATE,
                        System.currentTimeMillis(),
                        TinkarTerm.USER,
                        TinkarTerm.PRIMORDIAL_MODULE,
                        TinkarTerm.PRIMORDIAL_PATH);

        configureConceptsAndPatterns(starterData, uuidUtility);
        starterData.build(); //Natively writing data to spined array
        exportStarterData(); //exports starter data to pb.zip
        starterData.shutdown();
    }

    private static void configureConceptsAndPatterns(StarterData starterData, UUIDUtility uuidUtility){

        EntityProxy.Concept snomedIdentifier = EntityProxy.Concept.make("SNOMED CT Identifier Source", UuidUtil.fromSNOMED("900000000000294009"));
        starterData.concept(snomedIdentifier)
                .fullyQualifiedName("SNOMED CT integer identifier", TinkarTerm.PREFERRED)
                .synonym("SNOMED CT ID", TinkarTerm.PREFERRED)
                .synonym("SCTID", TinkarTerm.PREFERRED)
                .definition("Unique point of origin for identifier", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        EntityProxy.Concept snomedGrouper = EntityProxy.Concept.make("SNOMED CT Concept", uuidUtility.createUUID("SNOMED CT Concept"));
        starterData.concept(snomedGrouper)
                .fullyQualifiedName("SNOMED CT Concept", TinkarTerm.PREFERRED)
                .synonym("Health Concept", TinkarTerm.PREFERRED)
                .definition("A grouper concept that contains the SNOMED CT hierarchy", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedGrouper.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.ROOT_VERTEX))
                .build();

        EntityProxy.Concept snomedAuthor = EntityProxy.Concept.make("SNOMED CT LOINC Collaboration Author", uuidUtility.createUUID("SNOMED CT LOINC Collaboration Author"));
        starterData.concept(snomedAuthor)
                .fullyQualifiedName("SNOMED CT LOINC Collaboration Author", TinkarTerm.PREFERRED)
                .synonym("SNOMED CT LOINC Collaboration Author", TinkarTerm.PREFERRED)
                .definition("International Health Terminology Standards Development Organisation (IHTSDO) SNOMED CT and LOINC Author", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedAuthor.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();


        /* UUID from SNOMED Browser - This is the ECL query : '705114005 |LOINC Code System (qualifier value)|'  */
        EntityProxy.Concept loincIdentifier = EntityProxy.Concept.make("LOINC Number", UuidUtil.fromSNOMED("705114005"));
        starterData.concept(loincIdentifier)
                .fullyQualifiedName("LOINC Number", TinkarTerm.PREFERRED)
                .synonym("LOINC Num", TinkarTerm.PREFERRED)
                .synonym("LOINC ID",TinkarTerm.DESCRIPTION_TYPE)
                .synonym("LOINC Code", TinkarTerm.DESCRIPTION_TYPE)
                .definition("Unique point of origin for identifier", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //LOINC Long Common Name
        PublicId publicId = PublicIds.of(UUID.nameUUIDFromBytes("LOINC Long Name".getBytes()));
        EntityProxy.Concept loincCommonName = EntityProxy.Concept.make(publicId);
        starterData.concept(loincCommonName)
                .fullyQualifiedName("LOINC Long Common Name", TinkarTerm.PREFERRED)
                .synonym("LOINC Long Name", TinkarTerm.PREFERRED)
                .definition("The name associated with the selected code used to identify the test ordered or resulted", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincCommonName.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        //420336001 |Device descriptor (qualifier value)| is in SNOMED LOINC already.
        // Adding this because it was requested
        //Device Description
        EntityProxy.Concept deviceDescription = EntityProxy.Concept.make("Device Description", uuidUtility.createUUID("Device Description"));
        starterData.concept(deviceDescription)
                .fullyQualifiedName("Device Description", TinkarTerm.PREFERRED)
                .synonym("Device Name", TinkarTerm.PREFERRED)
                .definition("The name associated with a Device used to identify that device", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, deviceDescription.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        //Device Brand Name
        EntityProxy.Concept deviceBrand = EntityProxy.Concept.make("Device Brand Name", uuidUtility.createUUID("Device Brand Name"));
        starterData.concept(deviceBrand)
                .fullyQualifiedName("Device Description", TinkarTerm.PREFERRED)
                .synonym("Brand Name", TinkarTerm.PREFERRED)
                .definition("The Brand name associated with a Device", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, deviceBrand.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        //Device Version or Model
        EntityProxy.Concept deviceVersion = EntityProxy.Concept.make("Device Version or Model", uuidUtility.createUUID("Device Version or Model"));
        starterData.concept(deviceVersion)
                .fullyQualifiedName("Device Version or Model", TinkarTerm.PREFERRED)
                .synonym("Version or Model", TinkarTerm.PREFERRED)
                .definition("The Version or Model of a Device", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, deviceVersion.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.DESCRIPTION_TYPE))
                .build();

        //Vendor Reference ID
        EntityProxy.Concept vendorReference = EntityProxy.Concept.make("Vendor Reference ID", uuidUtility.createUUID("Vendor Reference ID"));
        starterData.concept(vendorReference)
                .fullyQualifiedName("Vendor Reference ID", TinkarTerm.PREFERRED)
                .synonym("Vendor Reference ID", TinkarTerm.PREFERRED)
                .definition("The vendor specified identifier used to locate the product information (e.g., insert) ", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.IDENTIFIER_SOURCE, vendorReference.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();


        //Authorized Settings
        EntityProxy.Concept authorizedSettings = EntityProxy.Concept.make("Authorized Settings", uuidUtility.createUUID("Authorized Settings"));
        starterData.concept(authorizedSettings)
                .fullyQualifiedName("Authorized Settings", TinkarTerm.PREFERRED)
                .definition("The setting type for which the test may be performed based on CLIA.  \"CLIA Complextity; multi-select allowed - choices are:\n" +
                        " H = high / M = medium/ W = waived / Home\"", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, authorizedSettings.asUuidArray()[0].toString())
                .identifier(TinkarTerm.MODEL_CONCEPT, authorizedSettings.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        //Adding this for completeness - should be used instead of creating a new Analyte concept for LIDR
        //Analyte  - This should be the "Analyte measured" already in SNOMEDLOINC:  246478007 |Analyte measured (attribute)|
        EntityProxy.Concept snomedLoincAnalyte = EntityProxy.Concept.make("Analyte measured (attribute)", UuidUtil.fromSNOMED("246478007"));
        starterData.concept(snomedLoincAnalyte)
                .fullyQualifiedName("Analyte measured (attribute)", TinkarTerm.PREFERRED)
                .synonym("Analyte measured", TinkarTerm.PREFERRED)
                .definition("What is specifically being measured by the test", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedLoincAnalyte.asUuidArray()[0].toString())
                .statedDefinition(List.of(snomedLoincAnalyte))
                .statedNavigation(List.of(snomedLoincAnalyte), List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        //Issuing Agency
        EntityProxy.Concept issueingAgency = EntityProxy.Concept.make("Issuing Agency", uuidUtility.createUUID("Issuing Agency"));
        starterData.concept(issueingAgency)
                .fullyQualifiedName("Issuing Agency", TinkarTerm.PREFERRED)
                .definition("Indicates the organization that establishes the UDI algorithm for assigning unique identifiers. ", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.MODEL_CONCEPT, issueingAgency.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        //Jurisdiction
        EntityProxy.Concept  jurisdictionConcept = EntityProxy.Concept.make("Jurisdiction", uuidUtility.createUUID("Jurisdiction"));
        starterData.concept(jurisdictionConcept)
                .fullyQualifiedName("Jurisdiction", TinkarTerm.PREFERRED)
                .definition("Indicates the authority regulating the test kit and Instrument", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.MODEL_CONCEPT, jurisdictionConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        /* No UUID from SNOMED Browser -  'SNOMED has no concept for Manufacturer yet'  */


        publicId = PublicIds.of(UUID.nameUUIDFromBytes("Manufacturer".getBytes()));
        EntityProxy.Concept manufacturerIdentifier = EntityProxy.Concept.make(publicId);
        starterData.concept(manufacturerIdentifier)
                .fullyQualifiedName("Manufacturer", TinkarTerm.PREFERRED)
                .definition("Defines the name of the legal entity that manufactures the test kit and Instrument", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.MODEL_CONCEPT, manufacturerIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        publicId = PublicIds.of(UUID.nameUUIDFromBytes("Manufactured By".getBytes()));
        EntityProxy.Concept manufacturedByIdentifier = EntityProxy.Concept.make(publicId);
        starterData.concept(manufacturedByIdentifier)
                .fullyQualifiedName("Manufactured By", TinkarTerm.PREFERRED)
                .definition("LIDR Axiom designating the manufacturer for a device", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.MODEL_CONCEPT, manufacturedByIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        publicId = PublicIds.of(UUID.nameUUIDFromBytes("Labeled By".getBytes()));
        EntityProxy.Concept labeledByIdentifier = EntityProxy.Concept.make(publicId);
        starterData.concept(labeledByIdentifier)
                .fullyQualifiedName("Labeled By", TinkarTerm.PREFERRED)
                .definition("LIDR Axiom designating the labeler for a device", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.MODEL_CONCEPT, labeledByIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        publicId = PublicIds.of(UUID.nameUUIDFromBytes("Allowed Result Set".getBytes()));
        EntityProxy.Concept allowedResultSetIdentifier = EntityProxy.Concept.make(publicId);
        starterData.concept(allowedResultSetIdentifier)
                .fullyQualifiedName("Allowed Result Set", TinkarTerm.PREFERRED)
                .definition("A Semantic concept specifying there is a set of allowed results for a Result Conformance", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.MODEL_CONCEPT, allowedResultSetIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.pattern( EntityProxy.Pattern.make("Qualitative Allowed Result Set Pattern", uuidUtility.createUUID("Qualitative Allowed Result Set Pattern")))
                .meaning(TinkarTerm.IDENTIFIER_SOURCE)
                .purpose(TinkarTerm.IDENTIFIER_SOURCE)
                .fieldDefinition(
                        allowedResultSetIdentifier,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .build();

        publicId = PublicIds.of(UUID.nameUUIDFromBytes("Allowed Result".getBytes()));
        EntityProxy.Concept allowedResultIdentifier = EntityProxy.Concept.make(publicId);
        starterData.concept(allowedResultIdentifier)
                .fullyQualifiedName("Allowed Result", TinkarTerm.PREFERRED)
                .definition("A Semantic concept specifying there is an allowed result format for a Result Conformance", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.MODEL_CONCEPT, allowedResultIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();



        EntityProxy.Concept deviceIDType = EntityProxy.Concept.make("Device ID Type", uuidUtility.createUUID("Device ID Type"));
        starterData.concept(deviceIDType)
                .fullyQualifiedName("Device ID Type", TinkarTerm.PREFERRED)
                .synonym("Device ID", TinkarTerm.PREFERRED)
                .definition("The type of identifier used for the test kit.", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, deviceIDType.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //TestKit UDI-DI
        EntityProxy.Concept testKitUDIDI = EntityProxy.Concept.make("TestKit UDI-DI", uuidUtility.createUUID("TestKit UDI-DI"));
        starterData.concept(testKitUDIDI)
                .fullyQualifiedName("TestKit UDI-DI", TinkarTerm.PREFERRED)
                .synonym("Test Kit Unique Device Identifier", TinkarTerm.PREFERRED)
                .definition("Indicates the unique device identifier for the kind of Test Kit.", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, testKitUDIDI.asUuidArray()[0].toString())
                .statedDefinition(List.of(deviceIDType))
                .build();

        //TestKit Name ID
        EntityProxy.Concept testKitNameID = EntityProxy.Concept.make("TestKit Name ID", uuidUtility.createUUID("TestKit Name ID"));
        starterData.concept(testKitNameID)
                .fullyQualifiedName("TestKit Name ID", TinkarTerm.PREFERRED)
                .synonym("Test Kit Unique Device Identifier", TinkarTerm.PREFERRED)
                .definition("A device identifier assigned by the manufacturer based on the Manufacturer name and Model of the Test Kit", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, testKitNameID.asUuidArray()[0].toString())
                .statedDefinition(List.of(deviceIDType))
                .build();

        //Instrument Equipment
        EntityProxy.Concept  instEquipmentConcept = EntityProxy.Concept.make("Instrument Equipment", uuidUtility.createUUID("Instrument Equipment"));
        starterData.concept(instEquipmentConcept)
                .fullyQualifiedName("Instrument Equipment", TinkarTerm.PREFERRED)
                .definition("A Device that is used to detect certain Analyte(s)", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.MODEL_CONCEPT, instEquipmentConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.pattern( EntityProxy.Pattern.make("Instrument Equipment Pattern", uuidUtility.createUUID("Instrument Equipment Pattern")))
                .meaning(TinkarTerm.IDENTIFIER_SOURCE)
                .purpose(TinkarTerm.IDENTIFIER_SOURCE)
                .fieldDefinition(
                        instEquipmentConcept,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .build();

        //Instrument Equipment Name ID
        EntityProxy.Concept  instEquipNameIDConcept = EntityProxy.Concept.make("Instrument Equipment Name ID", uuidUtility.createUUID("Instrument Equipment Name ID"));
        starterData.concept(instEquipNameIDConcept)
                .fullyQualifiedName("Instrument Equipment Name ID", TinkarTerm.PREFERRED)
                .synonym("Instrument Equipment Name ID", TinkarTerm.PREFERRED)
                .definition("Indicates the unique device identification by its manufacturer name and model of the instrument that is in commercial distribution.", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, instEquipNameIDConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(deviceIDType))
                .build();

        //Target - can't find the target related concept we want in SNOMEDLOINC
        publicId = PublicIds.of(UUID.nameUUIDFromBytes("Target".getBytes()));
        EntityProxy.Concept targetConcept = EntityProxy.Concept.make(publicId);
        starterData.concept(targetConcept)
                .fullyQualifiedName("Target", TinkarTerm.PREFERRED)
                .definition("The specific thing being captured/looked for that describes how the Analyte is being specifically meaured.", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.MODEL_CONCEPT, targetConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        fullyQualifiedNameToConceptMap.put("Target", targetConcept);

        /* UUID from SNOMED Browser - This is the ECL query : '57134006 |Instrument, device (physical object)|'  */
        publicId = PublicIds.of(UUID.nameUUIDFromBytes("Device".getBytes()));
        EntityProxy.Concept instrumentIdentifier = EntityProxy.Concept.make(publicId);
        //EntityProxy.Concept instrumentIdentifier = EntityProxy.Concept.make("Device", UuidUtil.fromSNOMED("57134006"));
        starterData.concept(instrumentIdentifier)
                .fullyQualifiedName("Device", TinkarTerm.PREFERRED)
                .definition("A thing made or adapted for a particular purpose, especially a piece of mechanical or electronic equipment", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.MODEL_CONCEPT, instrumentIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        publicId = PublicIds.of(UUID.nameUUIDFromBytes("LIDR Records".getBytes()));
        EntityProxy.Concept lidrRecords = EntityProxy.Concept.make(publicId);
        starterData.concept(lidrRecords)
                .fullyQualifiedName("LIDR Records", TinkarTerm.PREFERRED)
                .synonym("LIDR Records", TinkarTerm.PREFERRED)
                .definition("A model concept to hold LIDR records", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.MODEL_CONCEPT, lidrRecords.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();


        /* UUID from SNOMED Browser - This is the ECL query : '43222004 |Test kit method (procedure)|'  */
        // Maybe take this out since it is not just plain "Test kit".
        EntityProxy.Concept testKitMethodIdentifier = EntityProxy.Concept.make("Test kit method", UuidUtil.fromSNOMED("43222004"));
        starterData.concept(testKitMethodIdentifier)
                .fullyQualifiedName("Test kit method (procedure)", TinkarTerm.PREFERRED)
                .synonym("Test kit method", TinkarTerm.PREFERRED)
                .definition("Physical object (physical object)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, testKitMethodIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Test Kit
        EntityProxy.Concept testKitConcept = EntityProxy.Concept.make("Test Kit", uuidUtility.createUUID("Test Kit"));
        starterData.concept(testKitConcept)
                .fullyQualifiedName("Test Kit", TinkarTerm.PREFERRED)
                .synonym("Test Kit", TinkarTerm.PREFERRED)
                .definition("A Device that tests for a certain Analyte(s) by means of looking at Target(s)", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.MODEL_CONCEPT, testKitConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        configureResultConformanceAndAnalyteConcepts(starterData, uuidUtility);
        configureLidrSpecimenConcept(starterData, uuidUtility);
        configureAnalyteAndResultConformancePatterns(starterData, uuidUtility);
        configureLIDR_MetaDataPattern(starterData, uuidUtility);
        configurePhenomenonNavigation(starterData, uuidUtility);
        configureVendorCodeDescriptionPattern(starterData, uuidUtility);
        createTestPerformedPattern(starterData, uuidUtility);

    }

    private static void configureResultConformanceAndAnalyteConcepts(StarterData starterData, UUIDUtility uuidUtility) {

        /* UUID from SNOMED Browser - This is the ECL query : '246478007 |Analyte measured (attribute)|'  */
        PublicId publicId = PublicIds.of(UUID.nameUUIDFromBytes("Analyte".getBytes()));
        EntityProxy.Concept analyteConcept = EntityProxy.Concept.make(publicId);
        starterData.concept(analyteConcept)
                .fullyQualifiedName("Analyte", TinkarTerm.PREFERRED)
                .definition("What is specifically being measured by the test", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.MODEL_CONCEPT, analyteConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        fullyQualifiedNameToConceptMap.put("Analyte", analyteConcept);

        /* No UUID from SNOMED Browser -  'SNOMED has no concept for Result Conformance yet'  */
        publicId = PublicIds.of(UUID.nameUUIDFromBytes("Result Conformance".getBytes()));
        EntityProxy.Concept resultConformanceConcept = EntityProxy.Concept.make(publicId);
        starterData.concept(resultConformanceConcept)
                .fullyQualifiedName("Result Conformance", TinkarTerm.PREFERRED)
                .synonym("Result Constraints", TinkarTerm.PREFERRED)
                .definition("How the results for a specific test should be used/coded in relation to the results", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.MODEL_CONCEPT, resultConformanceConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        fullyQualifiedNameToConceptMap.put("Result Conformance", resultConformanceConcept);

        EntityProxy.Concept dataResultsType = EntityProxy.Concept.make("Data Results Type", uuidUtility.createUUID("Data Results Type"));
        starterData.concept(dataResultsType)
                .fullyQualifiedName("Data Results Type", TinkarTerm.PREFERRED)
                .synonym("Results Type", TinkarTerm.PREFERRED)
                .definition("Indicates the test results using a qualitative vs quantitative measure.", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.MODEL_CONCEPT, dataResultsType.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        fullyQualifiedNameToConceptMap.put("Data Results Type", dataResultsType);

        //Quantitative Data Results
        EntityProxy.Concept quantDataResults = EntityProxy.Concept.make("Quantitative Data Result", uuidUtility.createUUID("Quantitative Data Results"));
        starterData.concept(quantDataResults)
                .fullyQualifiedName("Quantitative Data Result", TinkarTerm.PREFERRED)
                .synonym("Quantitative", TinkarTerm.PREFERRED)
                .definition("Indicates the test results using a quantitative measure.", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, quantDataResults.asUuidArray()[0].toString())
                .statedDefinition(List.of(dataResultsType))
                .statedNavigation( List.of(quantDataResults) ,List.of(dataResultsType))
                .build();

        //Qualitative Data Results
        EntityProxy.Concept qualDataResults = EntityProxy.Concept.make("Qualitative Data Result", uuidUtility.createUUID("Qualitative Data Results"));
        starterData.concept(qualDataResults)
                .fullyQualifiedName("Qualitative Data Result", TinkarTerm.PREFERRED)
                .synonym("Qualitative", TinkarTerm.PREFERRED)
                .definition("Indicates the test results using a qualitative measure.Used to deterimine how the test results were interpreted and communicated.", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, qualDataResults.asUuidArray()[0].toString())
                .statedDefinition(List.of(dataResultsType))
                .statedNavigation( List.of(qualDataResults) ,List.of(dataResultsType))
                .build();

        //Data Results Type
        List<EntityProxy.Concept> resultConcepts = Arrays.asList( qualDataResults,quantDataResults );

        starterData.concept(dataResultsType)
                .statedNavigation(resultConcepts,List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(analyteConcept)
                .statedNavigation(List.of(analyteConcept),List.of(analyteConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.concept(resultConformanceConcept)
                .statedNavigation(List.of(resultConformanceConcept),List.of(resultConformanceConcept, TinkarTerm.MODEL_CONCEPT))
                .build();


    }

    private static void configureLidrSpecimenConcept(StarterData starterData, UUIDUtility uuidUtility) {

        /* No UUID from SNOMED Browser -  'SNOMED has no concept for LIDR Specimen yet'  */
        PublicId publicId = PublicIds.of(UUID.nameUUIDFromBytes("Specimen".getBytes()));
        EntityProxy.Concept lidrSpecimenConcept = EntityProxy.Concept.make(publicId);
        starterData.concept(lidrSpecimenConcept)
                .fullyQualifiedName("Specimen", TinkarTerm.PREFERRED)
                .definition("The specific object/thing being collected to be measured for specific Analyte(s)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.MODEL_CONCEPT, lidrSpecimenConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        fullyQualifiedNameToConceptMap.put("Specimen", lidrSpecimenConcept);

//        //In SNOMED-LOINC now: 371439000 |Specimen type (observable entity)| will be the source for below as per spreadsheet LIDRStarterData:
//        //Specimen Type
//        EntityProxy.Concept snomedSpecimenConcept = EntityProxy.Concept.make("Specimen Type", UuidUtil.fromSNOMED("371439000"));
//        starterData.concept(snomedSpecimenConcept)
//                .fullyQualifiedName("Specimen Type", TinkarTerm.PREFERRED)
//                .synonym("Specimen type (SNOMED CT®) at minimum",TinkarTerm.DESCRIPTION_TYPE)
//                .definition("\"From HL7: This field describes the precise nature of the entity that will be the source material for the observation.\n" +
//                        "This entry describes the specimen type, i.e. the \"\"what\"\" is being submitted\n", TinkarTerm.PREFERRED)
//                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedSpecimenConcept.asUuidArray()[0].toString())
//                .statedDefinition(List.of(lidrSpecimenConcept,snomedSpecimenConcept))
//                .statedNavigation( List.of(snomedSpecimenConcept) ,List.of( lidrSpecimenConcept))
//                .build();
//
//        //Specimen Type Modifier
//        EntityProxy.Concept specimenTypeModConcept = EntityProxy.Concept.make("Specimen Type Modifier", uuidUtility.createUUID("Specimen Type Modifier"));
//        starterData.concept(specimenTypeModConcept)
//                .fullyQualifiedName("Specimen Type Modifier", TinkarTerm.PREFERRED)
//                .definition("A modifying or qualifying description(s) about the specimen type", TinkarTerm.PREFERRED)
//                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, specimenTypeModConcept.asUuidArray()[0].toString())
//                .statedDefinition(List.of( lidrSpecimenConcept,specimenTypeModConcept))
//                .statedNavigation( List.of(specimenTypeModConcept) ,List.of( lidrSpecimenConcept))
//                .build();
//
//        //Specimen Source Site
//        EntityProxy.Concept specimenSourceSiteConcept = EntityProxy.Concept.make("Specimen Source Site", uuidUtility.createUUID("Specimen Source Site"));
//        starterData.concept(specimenSourceSiteConcept)
//                .fullyQualifiedName("Specimen Source Site", TinkarTerm.PREFERRED)
//                .synonym("Specimen source site (SNOMED CT®)", TinkarTerm.PREFERRED)
//                .definition("The source from which the specimen was obtained and describes where the specimen came from, i.e. the \"\"where\"\"", TinkarTerm.PREFERRED)
//                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, specimenSourceSiteConcept.asUuidArray()[0].toString())
//                .statedDefinition(List.of( lidrSpecimenConcept,specimenSourceSiteConcept))
//                .statedNavigation( List.of(specimenSourceSiteConcept) ,List.of(lidrSpecimenConcept))
//                .build();
//
//        //Specimen Collection Method  -  This is wrong SNOMED LOINC has Method I think?
//        publicId = PublicIds.of(UUID.nameUUIDFromBytes("Specimen Collection Method".getBytes()));
//        EntityProxy.Concept specimenCollectionMethodConcept = EntityProxy.Concept.make(publicId);
//        starterData.concept(specimenCollectionMethodConcept)
//                .fullyQualifiedName("Specimen Collection Method", TinkarTerm.PREFERRED)
//                .synonym("Specimen collection method (SNOMED CT®)", TinkarTerm.PREFERRED)
//                .definition("The procedure or process by which the specimen was collected.", TinkarTerm.PREFERRED)
//                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, specimenCollectionMethodConcept.asUuidArray()[0].toString())
//                .statedDefinition(List.of( lidrSpecimenConcept,specimenCollectionMethodConcept))
//                .statedNavigation( List.of(specimenCollectionMethodConcept) ,List.of( lidrSpecimenConcept))
//                .build();
//
//        //Specimen Additives
//        EntityProxy.Concept specimenAdditivesConcept = EntityProxy.Concept.make("Specimen Additives", uuidUtility.createUUID("Specimen Additives"));
//        starterData.concept(specimenAdditivesConcept)
//                .fullyQualifiedName("Specimen Additives", TinkarTerm.PREFERRED)
//                .synonym("Specimen additives (SNOMED CT®)", TinkarTerm.PREFERRED)
//                .definition("Specifies the actual additives used during the preparation of the specimen when performing the test.", TinkarTerm.PREFERRED)
//                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, specimenAdditivesConcept.asUuidArray()[0].toString())
//                .statedDefinition(List.of( lidrSpecimenConcept,specimenAdditivesConcept))
//                .statedNavigation( List.of(specimenAdditivesConcept) ,List.of( lidrSpecimenConcept))
//                .build();
//
//        List<EntityProxy.Concept> specimenConcepts = List.of(
//                lidrSpecimenConcept,  // These parent concepts need to be here to see desired hierarchy
//                snomedSpecimenConcept,specimenAdditivesConcept,
//                specimenSourceSiteConcept,specimenCollectionMethodConcept,
//                specimenTypeModConcept );
//
//        starterData.concept(lidrSpecimenConcept)
//                .statedNavigation(specimenConcepts,List.of(TinkarTerm.MODEL_CONCEPT))
//                .build();

    }

    private static void configureAnalyteAndResultConformancePatterns(StarterData starterData, UUIDUtility uuidUtility) {
        //Reference Ranges
        EntityProxy.Concept referenceRanges = EntityProxy.Concept.make("Reference Ranges", uuidUtility.createUUID("Reference Ranges"));
        starterData.concept(referenceRanges)
                .fullyQualifiedName("Reference Ranges", TinkarTerm.PREFERRED)
                .synonym("Range of Values", TinkarTerm.PREFERRED)
                .definition("A range of normal values expected for a healthy person based on a group of otherwise healthy people", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.MODEL_CONCEPT, referenceRanges.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        //Result Conformance Range Pattern
        starterData.pattern( EntityProxy.Pattern.make("Result Conformance Range Pattern", uuidUtility.createUUID("Result Conformance Range Pattern")))
                .meaning(TinkarTerm.IDENTIFIER_SOURCE)
                .purpose(TinkarTerm.IDENTIFIER_SOURCE)
                .fieldDefinition(
                        referenceRanges,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .build();

        //Detection Limit
        EntityProxy.Concept detectionLimit = EntityProxy.Concept.make("Detection Limit", uuidUtility.createUUID("Detection Limit"));
        starterData.concept(detectionLimit)
                .fullyQualifiedName("Detection Limit", TinkarTerm.PREFERRED)
                .synonym("Limit of Detection (LOD)", TinkarTerm.DESCRIPTION_TYPE)
                .definition("The limit of detection LOD (or detection limit, DL) is the lowest possible concentration at which the method can detect (but not quantify!) the analyte within the matrix with certain degree of confidence.", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.MODEL_CONCEPT, detectionLimit.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.pattern( EntityProxy.Pattern.make("Analyte Range Pattern", uuidUtility.createUUID("Analyte Range Pattern")))
                .meaning(TinkarTerm.IDENTIFIER_SOURCE)
                .purpose(TinkarTerm.IDENTIFIER_SOURCE)
                .fieldDefinition(
                        referenceRanges,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        detectionLimit,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .build();
    }

    private static void configureVendorCodeDescriptionPattern(StarterData starterData, UUIDUtility uuidUtility){
        //Vendor Code
        EntityProxy.Concept vendorCode = EntityProxy.Concept.make("Vendor Code", uuidUtility.createUUID("Vendor Code"));
        starterData.concept(vendorCode)
                .fullyQualifiedName("Vendor Code", TinkarTerm.PREFERRED)
                .definition("A code used by a Vendor to specify an Analyte, Target, Specimen, or Results Conformance", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.MODEL_CONCEPT, vendorCode.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        //Vendor Description
        EntityProxy.Concept vendorDescription = EntityProxy.Concept.make("Vendor Description", uuidUtility.createUUID("Vendor Description"));
        starterData.concept(vendorDescription)
                .fullyQualifiedName("Vendor Description", TinkarTerm.PREFERRED)
                .definition("A description used by a Vendor to specify an Analyte, Target, Specimen, or Results Conformance", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.MODEL_CONCEPT, vendorDescription.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();



        starterData.pattern( EntityProxy.Pattern.make("Vendor Code/Description Pattern", uuidUtility.createUUID("Vendor Code/Description Pattern")))
                .meaning(TinkarTerm.IDENTIFIER_SOURCE)
                .purpose(TinkarTerm.IDENTIFIER_SOURCE)
                .fieldDefinition(
                        vendorCode,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        vendorDescription,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .build();


    }

    private static void configureLIDR_MetaDataPattern(StarterData starterData, UUIDUtility uuidUtility) {

        PublicId publicId = PublicIds.of(UUID.nameUUIDFromBytes("LIDR Metadata".getBytes()));
        EntityProxy.Concept lidrMetadataConcept = EntityProxy.Concept.make(publicId);
        starterData.concept(lidrMetadataConcept)
                .fullyQualifiedName("LIDR Metadata", TinkarTerm.PREFERRED)
                .synonym("LIDR Metadata", TinkarTerm.PREFERRED)
                .definition("The semantic purpose and meaning to describe LOINC Values assigned and reviewed by the LIDR Comittee and a Vendor", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.MODEL_CONCEPT, lidrMetadataConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();
        //LOINC Version ID
        EntityProxy.Concept loincVersionID = EntityProxy.Concept.make("LOINC Version ID", uuidUtility.createUUID("LOINC Version ID"));
        starterData.concept(loincVersionID)
                .fullyQualifiedName("LOINC Version ID", TinkarTerm.PREFERRED)
                .definition("The version of LOINC used to establish the LOINC mapping for the described test", TinkarTerm.SOURCE_CONTENT_VERSION)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincVersionID.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataConcept))
                .statedNavigation(List.of(loincVersionID),List.of(lidrMetadataConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Date Mapping Performed
        EntityProxy.Concept dateMappingPerformed = EntityProxy.Concept.make("Date Mapping Performed", uuidUtility.createUUID("Date Mapping Performed"));
        starterData.concept(dateMappingPerformed)
                .fullyQualifiedName("Date Mapping Performed", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, dateMappingPerformed.asUuidArray()[0].toString())
                .definition("Date the LOINC mapping was performed - this will help understand the temporal context and if review is needed with a new LOINC release", TinkarTerm.SOURCE_RELEASE_DATE)
                .statedDefinition(List.of(lidrMetadataConcept))
                .statedNavigation(List.of(dateMappingPerformed),List.of(lidrMetadataConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Source of Entry
        EntityProxy.Concept sourceOfEntry = EntityProxy.Concept.make("Source of Entry", uuidUtility.createUUID("Source of Entry"));
        starterData.concept(sourceOfEntry)
                .fullyQualifiedName("Source of Entry", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, sourceOfEntry.asUuidArray()[0].toString())
                .definition("Name of the organization that submitted the data for the LIDR Record", TinkarTerm.NAME)
                .statedDefinition(List.of(lidrMetadataConcept))
                .statedNavigation(List.of(sourceOfEntry),List.of(lidrMetadataConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        EntityProxy.Concept committeeReviewed = EntityProxy.Concept.make("Committee Reviewed", uuidUtility.createUUID("Committee Reviewed"));
        starterData.concept(committeeReviewed)
                .fullyQualifiedName("Committee Reviewed", TinkarTerm.PREFERRED)
                .definition("Lists the name of the committee who reviewed the data in this row for accuracy in representing the package insert content and the LOINC (and other standard code) mapping Considered NOT REVIEWED when blank For now should only be \"LIDR\"", TinkarTerm.NAME)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, committeeReviewed.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataConcept))
                .statedNavigation(List.of(committeeReviewed),List.of(lidrMetadataConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Vendor Reviewed
        EntityProxy.Concept vendorReviewed = EntityProxy.Concept.make("Vendor Reviewed", uuidUtility.createUUID("Vendor Reviewed"));
        starterData.concept(vendorReviewed)
                .fullyQualifiedName("Vendor Reviewed", TinkarTerm.PREFERRED)
                .synonym("Vendor Reviewed", TinkarTerm.PREFERRED)
                .definition("\"Lists the name of the manufacturer who reviewed the data in this row for accuracy in representing the package insert content and the LOINC (and other standard code) mapping\n" +
                        " Should match the name listed in Manufacturer\n" +
                        " Considered NOT REVIEWED when blank\"", TinkarTerm.NAME)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, vendorReviewed.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataConcept))
                .statedNavigation(List.of(vendorReviewed),List.of(lidrMetadataConcept,TinkarTerm.MODEL_CONCEPT))
                .build();


        //Test Ordered
        EntityProxy.Concept testOrdered = EntityProxy.Concept.make("Test Ordered", uuidUtility.createUUID("Test Ordered"));
        starterData.concept(testOrdered)
                .fullyQualifiedName("Test Ordered", TinkarTerm.PREFERRED)
                .synonym("Test Ordered analyte/observable (LOINC ®™)", TinkarTerm.PREFERRED)
                .definition("Provides the specific test that was ordered for machine interpretation and can be checked against the test that was performed.", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, testOrdered.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataConcept))
                .statedNavigation(List.of(testOrdered),List.of(lidrMetadataConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Test Performed
        EntityProxy.Concept testPerformed = EntityProxy.Concept.make("Test Performed", uuidUtility.createUUID("Test Performed"));
        starterData.concept(testPerformed)
                .fullyQualifiedName("Test Performed", TinkarTerm.PREFERRED)
                .synonym("Test Performed analyte/observable (LOINC ®™)", TinkarTerm.PREFERRED)
                .definition("Provides the specific test that was performed for machine interpretation and can be checked against the test that was ordered.", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, testPerformed.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataConcept))
                .statedNavigation(List.of(testPerformed),List.of(lidrMetadataConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        fullyQualifiedNameToConceptMap.put("Test Performed", testPerformed);


        //Vendor Comment
        EntityProxy.Concept vendorComment = EntityProxy.Concept.make("Vendor Comment", uuidUtility.createUUID("Vendor Comment"));
        starterData.concept(vendorComment)
                .fullyQualifiedName("Vendor Comment", TinkarTerm.PREFERRED)
                .synonym("Vendor Comment (Additional Information for Coding of LOINC attributes)", TinkarTerm.PREFERRED)
                .definition("A human-readable text clarification about the test being performed", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, vendorComment.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataConcept))
                .statedNavigation(List.of(vendorComment),List.of(lidrMetadataConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        List<EntityProxy.Concept> metaDataConcepts = List.of(
                testOrdered,  // These parent concepts need to be here to see desired hierarchy
                testPerformed,dateMappingPerformed,
                loincVersionID,sourceOfEntry,
                committeeReviewed, vendorReviewed, vendorComment  );

        starterData.concept(lidrMetadataConcept)
                .statedNavigation(metaDataConcepts,List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.pattern( EntityProxy.Pattern.make("LIDR MetaData Pattern", uuidUtility.createUUID("LIDR MetaData Pattern")))
                .meaning(TinkarTerm.IDENTIFIER_SOURCE)
                .purpose(TinkarTerm.IDENTIFIER_SOURCE)
                .fieldDefinition(
                        dateMappingPerformed,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        loincVersionID,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        sourceOfEntry,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        committeeReviewed,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        vendorReviewed,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        vendorComment,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .build();

        starterData.pattern( EntityProxy.Pattern.make("Diagnostic Device Pattern", uuidUtility.createUUID("Diagnostic Device Pattern")))
                .meaning(TinkarTerm.IDENTIFIER_SOURCE)
                .purpose(TinkarTerm.IDENTIFIER_SOURCE)
                .fieldDefinition(
                        testOrdered,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .build();
    }

    private static void configurePhenomenonNavigation(StarterData starterData, UUIDUtility uuidUtility) {
        /* PHENOMENON - as specified by TE team for LOINC axes   */
        List<EntityProxy.Concept> lidrPhenomenonConcepts = new ArrayList<>();
        EntityProxy.Concept phenomenonConcept = EntityProxy.Concept.make("PHENOMENON", uuidUtility.createUUID("PHENOMENON"));
        starterData.concept(phenomenonConcept)
                .fullyQualifiedName("PHENOMENON", TinkarTerm.PREFERRED)
                .synonym("PHENOMENON", TinkarTerm.PREFERRED)
                .definition("The LOINC axes concepts", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, phenomenonConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .statedNavigation(lidrPhenomenonConcepts,List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        //Component   From SNOMED-LOINC : 246093002  |Component (attribute)|
        EntityProxy.Concept loincComponent = EntityProxy.Concept.make("Component", UuidUtil.fromSNOMED("246093002"));
        starterData.concept(loincComponent)
                .fullyQualifiedName("Component (attribute)", TinkarTerm.PREFERRED)
                .definition("The substance or entity being measured or observed", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincComponent.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(loincComponent),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Property   From SNOMED-LOINC : 370130000 |Property (attribute)|
        EntityProxy.Concept loincProperty = EntityProxy.Concept.make("Property", UuidUtil.fromSNOMED("370130000"));
        starterData.concept(loincProperty)
                .fullyQualifiedName("Property (attribute)", TinkarTerm.PREFERRED)
                .definition("The characteristic or attribute of the analyte, distinguishes between different kinds of quantities relating to the same substance, e.g. Mass versus Counts vs time duration", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincProperty.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(loincProperty),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Time Aspect   From SNOMED-LOINC : 370134009 |Time aspect (attribute)|
        EntityProxy.Concept loincTimeAspect = EntityProxy.Concept.make("Time Aspect", UuidUtil.fromSNOMED("370134009"));
        starterData.concept(loincTimeAspect)
                .fullyQualifiedName("Time Aspect", TinkarTerm.PREFERRED)
                .synonym("Time; Timing", TinkarTerm.PREFERRED)
                .definition("One can either measure a Property at a moment (point) in time or measure it over a time interval and integrate, in the mathematical sense, over time.The interval of time over which an observation was made. (e.g., Point in time vs 24 hrs collection)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincTimeAspect.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(loincTimeAspect),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Direct site   From SNOMED-LOINC : 704327008 |Direct site (attribute)|
        EntityProxy.Concept loincDirectSite = EntityProxy.Concept.make("System", UuidUtil.fromSNOMED("704327008"));
        starterData.concept(loincDirectSite)
                .fullyQualifiedName("System", TinkarTerm.PREFERRED)
                .synonym("System", TinkarTerm.PREFERRED)
                .definition("Refers to the specific anatomical location or site where a particular observation or test is performed.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincDirectSite.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(loincDirectSite),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Scale type   From SNOMED-LOINC : 370132008 |Scale type (attribute)|
        EntityProxy.Concept loincScaleType = EntityProxy.Concept.make("Scale type", UuidUtil.fromSNOMED("370132008"));
        starterData.concept(loincScaleType)
                .fullyQualifiedName("Scale type (attribute)", TinkarTerm.PREFERRED)
                .synonym("Scale", TinkarTerm.PREFERRED)
                .definition("How the observation value is quantified or expressed. Type of scale specifies the scale of the measure. The following scale types are defined: Quantitative (Qn), Ordinal (Ord), Nominal (Nom), Narrative (Nar)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincScaleType.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(loincScaleType),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Method From SNOMED-LOINC :  From SNOMED-LOINC : 260686004 |Method (attribute)|
        EntityProxy.Concept methodType = EntityProxy.Concept.make("Method", UuidUtil.fromSNOMED("260686004"));
        starterData.concept(methodType)
                .fullyQualifiedName("Method (attribute)", TinkarTerm.PREFERRED)
                .synonym("Technique", TinkarTerm.PREFERRED)
                .definition("A high-level classification of how the observation was made. What methodology is being used to make the measurement. Only needed when the technique affects the clinical interpretation of the results.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, methodType.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(methodType),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Units from SNOMED-LOINC :  From SNOMED-LOINC : 246514001 |Units (attribute)|
        EntityProxy.Concept loincUnitsType = EntityProxy.Concept.make("Units", UuidUtil.fromSNOMED("246514001"));
        starterData.concept(loincUnitsType)
                .fullyQualifiedName("Units (attribute)", TinkarTerm.PREFERRED)
                .synonym("Units of Measure", TinkarTerm.PREFERRED)
                .definition("Provides expected UCUM (Unified Code for Units of Measure) units of measure for  values of an observation", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincUnitsType.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(loincUnitsType),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        starterData.pattern( EntityProxy.Pattern.make("Quantitative Allowed Result Pattern", uuidUtility.createUUID("Quantitative Allowed Result Pattern")))
                .meaning(TinkarTerm.IDENTIFIER_SOURCE)
                .purpose(TinkarTerm.IDENTIFIER_SOURCE)
                .fieldDefinition(
                        loincUnitsType,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        TinkarTerm.STRING,
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .build();


        lidrPhenomenonConcepts.addAll(List.of(loincComponent,loincProperty,loincTimeAspect,
                loincDirectSite, loincScaleType, methodType,loincUnitsType));

        starterData.concept(phenomenonConcept)
                .statedNavigation(lidrPhenomenonConcepts,List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

    }

    private static void createTestPerformedPattern(StarterData starterData, UUIDUtility uuidUtility)
    {

        starterData.pattern( EntityProxy.Pattern.make("Test Performed Pattern (LIDR Record)", uuidUtility.createUUID("Test Performed Pattern (LIDR Record)")))
                .meaning(TinkarTerm.IDENTIFIER_SOURCE)
                .purpose(TinkarTerm.IDENTIFIER_SOURCE)
                .fieldDefinition(
                        fullyQualifiedNameToConceptMap.get("Test Performed"),
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        fullyQualifiedNameToConceptMap.get("Data Results Type"),
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        fullyQualifiedNameToConceptMap.get("Analyte"),
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_FIELD)
                .fieldDefinition(
                        fullyQualifiedNameToConceptMap.get("Target"),
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_ID_SET_FIELD)
                .fieldDefinition(
                        fullyQualifiedNameToConceptMap.get("Specimen"),
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_ID_SET_FIELD)
                .fieldDefinition(
                        fullyQualifiedNameToConceptMap.get("Result Conformance"),
                        TinkarTerm.IDENTIFIER_SOURCE,
                        TinkarTerm.COMPONENT_ID_SET_FIELD)
                .build();

    }


    private static void buildSampleLIDRConcept(StarterData starterData, List<EntityProxy.Concept> definitionConcepts,
                                                EntityProxy.Concept lidrRecordConcept)
    {
        UUIDUtility uuidUtility = new UUIDUtility();
        EntityProxy.Concept lidrConceptExample = EntityProxy.Concept.make("Abbot Labs - AdviseDX SARS-CoV-2 IgM Architect i1000SR", uuidUtility.createUUID("Abbot Labs - AdviseDX SARS-CoV-2 IgM Architect i1000SR"));
        starterData.concept(lidrConceptExample)
                .fullyQualifiedName("Abbot Labs - AdviseDX SARS-CoV-2 IgM Architect i1000SR", TinkarTerm.PREFERRED)
                .synonym("Abbot Labs - AdviseDX SARS-CoV-2 IgM Architect i1000SR", TinkarTerm.PREFERRED)
                .definition("Abbot Labs - AdviseDX SARS-CoV-2 IgM Architect i1000SR", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, lidrConceptExample.asUuidArray()[0].toString())
                .statedDefinition(definitionConcepts)
                .statedNavigation(definitionConcepts,Arrays.asList(lidrRecordConcept))
                .build();
    }

    private static void exportStarterData(){
        ExportEntitiesController exportEntitiesController = new ExportEntitiesController();
        try {
            exportEntitiesController.export(exportFile).get();
        } catch (ExecutionException | InterruptedException e){
            e.printStackTrace();
        }
    }
}
