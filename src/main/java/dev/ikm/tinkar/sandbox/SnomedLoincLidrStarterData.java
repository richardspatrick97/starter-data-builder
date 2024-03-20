package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.logging.Logger;
public class SnomedLoincLidrStarterData {
    private static final Logger LOG = Logger.getLogger(SnomedLoincLidrStarterData.class.getSimpleName());

    private static File exportDataStore;
    private static File exportFile;
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
                .definition("Unique point of origin for identifier", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //LOINC Long Common Name
        EntityProxy.Concept loincCommonName = EntityProxy.Concept.make("LOINC Long Common Name", uuidUtility.createUUID("LOINC Long Common Name"));
        starterData.concept(loincCommonName)
                .fullyQualifiedName("LOINC Long Common Name", TinkarTerm.PREFERRED)
                .synonym("LOINC Long Name", TinkarTerm.PREFERRED)
                .definition("The name associated with the selected code used to identify the test ordered or resulted", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincCommonName.asUuidArray()[0].toString())
                .build();

        //420336001 |Device descriptor (qualifier value)| is in SNOMED LOINC already.
        // Adding this because it was requested
        //Device Description
        EntityProxy.Concept deviceDescription = EntityProxy.Concept.make("Device Description", uuidUtility.createUUID("Device Description"));
        starterData.concept(deviceDescription)
                .fullyQualifiedName("Device Description", TinkarTerm.PREFERRED)
                .synonym("Device Name", TinkarTerm.PREFERRED)
                .definition("The name associated with a Device used to identify that device", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.DESCRIPTION_TYPE, deviceDescription.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Device Brand Name
        EntityProxy.Concept deviceBrand = EntityProxy.Concept.make("Device Brand Name", uuidUtility.createUUID("Device Brand Name"));
        starterData.concept(deviceBrand)
                .fullyQualifiedName("Device Description", TinkarTerm.PREFERRED)
                .synonym("Brand Name", TinkarTerm.PREFERRED)
                .definition("The Brand name associated with a Device", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.DESCRIPTION_TYPE, deviceBrand.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Device Version or Model
        EntityProxy.Concept deviceVersion = EntityProxy.Concept.make("Device Version or Model", uuidUtility.createUUID("Device Version or Model"));
        starterData.concept(deviceVersion)
                .fullyQualifiedName("Device Version or Model", TinkarTerm.PREFERRED)
                .synonym("Version or Model", TinkarTerm.PREFERRED)
                .definition("The Version or Model of a Device", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.DESCRIPTION_TYPE, deviceVersion.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
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



        //Vendor Code
        EntityProxy.Concept vendorCode = EntityProxy.Concept.make("Vendor Code", uuidUtility.createUUID("Vendor Code"));
        starterData.concept(vendorCode)
                .fullyQualifiedName("Vendor Code", TinkarTerm.PREFERRED)
                .synonym("Vendor Code", TinkarTerm.PREFERRED)
                .definition("A code used by a Vendor to specify an Analyte, Target, Specimen, or Results Conformance", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedAuthor.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Vendor Description
        EntityProxy.Concept vendorDescription = EntityProxy.Concept.make("Vendor Description", uuidUtility.createUUID("Vendor Description"));
        starterData.concept(vendorDescription)
                .fullyQualifiedName("Vendor Description", TinkarTerm.PREFERRED)
                .synonym("Vendor Description", TinkarTerm.PREFERRED)
                .definition("A description used by a Vendor to specify an Analyte, Target, Specimen, or Results Conformance", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, vendorCode.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();



        //LIDR Concept
        EntityProxy.Concept lidrConcept = EntityProxy.Concept.make("LIDR Concept", uuidUtility.createUUID("LIDR Concept"));
        starterData.concept(lidrConcept)
                .fullyQualifiedName("LIDR Concept", TinkarTerm.PREFERRED)
                .synonym("LIDR Record", TinkarTerm.PREFERRED)
                .definition("A concept that contain and relates all Labratory Interoperability Device Reference (LIDR) (Analyte(s), Target(s), Specimen(s), and Result Conformance(s)) to a Device", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, lidrConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .statedNavigation(List.of(lidrConcept),List.of(TinkarTerm.MODEL_CONCEPT))
                .build();



        //Authorized Settings
        EntityProxy.Concept authorizedSettings = EntityProxy.Concept.make("Authorized Settings", uuidUtility.createUUID("Authorized Settings"));
        starterData.concept(authorizedSettings)
                .fullyQualifiedName("Authorized Settings", TinkarTerm.PREFERRED)
                .synonym("Authorized Settings", TinkarTerm.PREFERRED)
                .definition("The setting type for which the test may be performed based on CLIA.  \"CLIA Complextity; multi-select allowed - choices are:\n" +
                        " H = high / M = medium/ W = waived / Home\"", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, authorizedSettings.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrConcept))
                .statedNavigation(List.of(authorizedSettings),List.of(lidrConcept))
                .build();

        //Component, Property, Time Aspect, Direct Site, Scale Type, Method, Units are concepts that
        //will be brought over from the SNOMED-International dataset because LOINC Concepts used those
        //Concepts to build out the SNOMED-LOINC concepts.




        //Quantitative Data Results
        EntityProxy.Concept quantDataResults = EntityProxy.Concept.make("Quantitative Data Results", uuidUtility.createUUID("Quantitative Data Results"));
        starterData.concept(quantDataResults)
                .fullyQualifiedName("Quantitative Data Results", TinkarTerm.PREFERRED)
                .synonym("Quantitative", TinkarTerm.PREFERRED)
                .definition("Indicates the test results using a quantitative measure.", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, quantDataResults.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Qualitative Data Results
        EntityProxy.Concept qualDataResults = EntityProxy.Concept.make("Qualitative Data Results", uuidUtility.createUUID("Qualitative Data Results"));
        starterData.concept(qualDataResults)
                .fullyQualifiedName("Qualitative Data Results", TinkarTerm.PREFERRED)
                .synonym("Qualitative", TinkarTerm.PREFERRED)
                .definition("Indicates the test results using a qualitative measure.Used to deterimine how the test results were interpreted and communicated.", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, qualDataResults.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Data Results Type
        List<EntityProxy.Concept> resultConcepts = Arrays.asList(qualDataResults,quantDataResults );
        EntityProxy.Concept dataResultsType = EntityProxy.Concept.make("Data Results Type", uuidUtility.createUUID("Data Results Type"));
        starterData.concept(dataResultsType)
                .fullyQualifiedName("Data Results Type", TinkarTerm.PREFERRED)
                .synonym("Results Type", TinkarTerm.PREFERRED)
                .definition("Indicates the test results using a qualitative vs quantitative measure.", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, dataResultsType.asUuidArray()[0].toString())
                .statedDefinition(resultConcepts)
                .statedNavigation(resultConcepts,List.of(dataResultsType))
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
                .statedNavigation(List.of(snomedLoincAnalyte), List.of(lidrConcept))
                .build();

        //In SNOMED-LOINC now: 371439000 |Specimen type (observable entity)| will be the source for below as per spreadsheet LIDRStarterData:
        //Specimen Type
        EntityProxy.Concept specimenConcept = EntityProxy.Concept.make("Specimen Type", UuidUtil.fromSNOMED("371439000"));
        starterData.concept(specimenConcept)
                .fullyQualifiedName("Specimen Type", TinkarTerm.PREFERRED)
                .synonym("Specimen type (SNOMED CT®) at minimum",TinkarTerm.DESCRIPTION_TYPE)
                .definition("\"From HL7: This field describes the precise nature of the entity that will be the source material for the observation.\n" +
                        "This entry describes the specimen type, i.e. the \"\"what\"\" is being submitted\n", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, specimenConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Specimen Type Modifier
        EntityProxy.Concept specimenTypeModConcept = EntityProxy.Concept.make("Specimen Type Modifier", uuidUtility.createUUID("Specimen Type Modifier"));
        starterData.concept(specimenTypeModConcept)
                .fullyQualifiedName("Specimen Type Modifier", TinkarTerm.PREFERRED)
                .synonym("Specimen Type Modifier", TinkarTerm.PREFERRED)
                .definition("A modifying or qualifying description(s) about the specimen type", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, specimenTypeModConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Specimen Source Site
        EntityProxy.Concept specimenSourceSiteConcept = EntityProxy.Concept.make("Specimen Source Site", uuidUtility.createUUID("Specimen Source Site"));
        starterData.concept(specimenSourceSiteConcept)
                .fullyQualifiedName("Specimen Source Site", TinkarTerm.PREFERRED)
                .synonym("Specimen source site (SNOMED CT®)", TinkarTerm.PREFERRED)
                .definition("The source from which the specimen was obtained and describes where the specimen came from, i.e. the \"\"where\"\"", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, specimenSourceSiteConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Specimen Collection Method  -  This is wrong SNOMED LOINC has Method I think?
        EntityProxy.Concept specimenCollectionMethodConcept = EntityProxy.Concept.make("Specimen Collection Method", uuidUtility.createUUID("Specimen Collection Method"));
        starterData.concept(specimenCollectionMethodConcept)
                .fullyQualifiedName("Specimen Collection Method", TinkarTerm.PREFERRED)
                .synonym("Specimen collection method (SNOMED CT®)", TinkarTerm.PREFERRED)
                .definition("The procedure or process by which the specimen was collected.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, specimenCollectionMethodConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Specimen Additives
        EntityProxy.Concept specimenAdditivesConcept = EntityProxy.Concept.make("Specimen Additives", uuidUtility.createUUID("Specimen Additives"));
        starterData.concept(specimenAdditivesConcept)
                .fullyQualifiedName("Specimen Additives", TinkarTerm.PREFERRED)
                .synonym("Specimen additives (SNOMED CT®)", TinkarTerm.PREFERRED)
                .definition("Specifies the actual additives used during the preparation of the specimen when performing the test.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, specimenAdditivesConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        List<EntityProxy.Concept> specimenConcepts = List.of(lidrConcept, specimenConcept,specimenAdditivesConcept,
                specimenSourceSiteConcept,specimenCollectionMethodConcept, specimenTypeModConcept );

        /* No UUID from SNOMED Browser -  'SNOMED has no concept for LIDR Specimen yet'  */
        EntityProxy.Concept lidrSpecimenConcept = EntityProxy.Concept.make("LIDR Specimen", uuidUtility.createUUID("LIDR Specimen"));
        starterData.concept(lidrSpecimenConcept)
                .fullyQualifiedName("LIDR Specimen", TinkarTerm.PREFERRED)
                .synonym("LIDR Specimen", TinkarTerm.PREFERRED)
                .definition("Laboratory Interoperability Device Reference Result Specimen", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, lidrSpecimenConcept.asUuidArray()[0].toString())
                .statedDefinition(specimenConcepts)
                .statedNavigation(specimenConcepts,List.of(lidrConcept))
                .build();



        //TestKit UDI-DI
        EntityProxy.Concept testKitUDIDI = EntityProxy.Concept.make("TestKit UDI-DI", uuidUtility.createUUID("TestKit UDI-DI"));
        starterData.concept(testKitUDIDI)
                .fullyQualifiedName("TestKit UDI-DI", TinkarTerm.PREFERRED)
                .synonym("Test Kit Unique Device Identifier", TinkarTerm.PREFERRED)
                .definition("Indicates the unique device identifier for the kind of Test Kit.", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, testKitUDIDI.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //TestKit Name ID
        EntityProxy.Concept testKitNameID = EntityProxy.Concept.make("TestKit Name ID", uuidUtility.createUUID("TestKit Name ID"));
        starterData.concept(testKitNameID)
                .fullyQualifiedName("TestKit Name ID", TinkarTerm.PREFERRED)
                .synonym("Test Kit Unique Device Identifier", TinkarTerm.PREFERRED)
                .definition("A device identifier assigned by the manufacturer based on the Manufacturer name and Model of the Test Kit", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, testKitNameID.asUuidArray()[0].toString())
                .statedDefinition(List.of(testKitNameID))
                .build();

        //Issuing Agency
        EntityProxy.Concept issueingAgency = EntityProxy.Concept.make("Issuing Agency", uuidUtility.createUUID("Issuing Agency"));
        starterData.concept(issueingAgency)
                .fullyQualifiedName("Issuing Agency", TinkarTerm.PREFERRED)
                .synonym("Issuing Agency", TinkarTerm.PREFERRED)
                .definition("Indicates the organization that establishes the UDI algorithm for assigning unique identifiers. ", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, issueingAgency.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrConcept))
                .statedNavigation(List.of(issueingAgency),List.of(lidrConcept))
                .build();

        //Jurisdiction
        EntityProxy.Concept  jurisdictionConcept = EntityProxy.Concept.make("Jurisdiction", uuidUtility.createUUID("Jurisdiction"));
        starterData.concept(jurisdictionConcept)
                .fullyQualifiedName("Jurisdiction", TinkarTerm.PREFERRED)
                .synonym("Jurisdiction", TinkarTerm.PREFERRED)
                .definition("Indicates the authority regulating the test kit and Instrument", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, jurisdictionConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrConcept))
                .statedNavigation(List.of(jurisdictionConcept),List.of(lidrConcept))
                .build();

        /* No UUID from SNOMED Browser -  'SNOMED has no concept for Manufacturer yet'  */
        EntityProxy.Concept manufacturerIdentifier = EntityProxy.Concept.make("Manufacturer", uuidUtility.createUUID("LIDR Device Manufacturer"));
        starterData.concept(manufacturerIdentifier)
                .fullyQualifiedName("Manufacturer", TinkarTerm.PREFERRED)
                .synonym("Manufacturer (company)", TinkarTerm.PREFERRED)
                .definition("Defines the name of the legal entity that manufactures the test kit and Instrument", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.MODEL_CONCEPT, manufacturerIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        //Instrument Equipment UDI-DI
        EntityProxy.Concept  instEquipUDI_DiConcept = EntityProxy.Concept.make("Instrument Equipment UDI-DI", uuidUtility.createUUID("Instrument Equipment UDI-DI"));
        starterData.concept(instEquipUDI_DiConcept)
                .fullyQualifiedName("Instrument Equipment UDI-DI", TinkarTerm.PREFERRED)
                .synonym("Instrument Equipment Unique Device Identifier", TinkarTerm.PREFERRED)
                .definition("Indicates the unique device identifier for the kind of instrument.", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, instEquipUDI_DiConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Instrument Equipment Name ID
        EntityProxy.Concept  instEquipNameIDConcept = EntityProxy.Concept.make("Instrument Equipment Name ID", uuidUtility.createUUID("Instrument Equipment Name ID"));
        starterData.concept(instEquipNameIDConcept)
                .fullyQualifiedName("Instrument Equipment Name ID", TinkarTerm.PREFERRED)
                .synonym("Instrument Equipment Name ID", TinkarTerm.PREFERRED)
                .definition("Indicates the unique device identification by its manufacturer name and model of the instrument that is in commercial distribution.", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, instEquipNameIDConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Device ID Type
        List<EntityProxy.Concept> deviceIdConcepts = Arrays.asList(testKitUDIDI,testKitNameID,
                instEquipUDI_DiConcept, instEquipNameIDConcept, manufacturerIdentifier);
        EntityProxy.Concept deviceIDType = EntityProxy.Concept.make("Device ID Type", uuidUtility.createUUID("Device ID Type"));
        starterData.concept(deviceIDType)
                .fullyQualifiedName("Device ID Type", TinkarTerm.PREFERRED)
                .synonym("Device ID", TinkarTerm.PREFERRED)
                .definition("The type of identifier used for the test kit.", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, deviceIDType.asUuidArray()[0].toString())
                .statedDefinition(deviceIdConcepts)
                .statedNavigation(deviceIdConcepts,List.of(lidrConcept))
                .build();

        //Vendor Code/Description Type
        EntityProxy.Concept  vendorOrDecriptionTypeConcept = EntityProxy.Concept.make("Vendor Code/Description Type", uuidUtility.createUUID("Vendor Code/Description Type"));
        starterData.concept(vendorOrDecriptionTypeConcept)
                .fullyQualifiedName("Vendor Code/Description Type", TinkarTerm.PREFERRED)
                .synonym("Vendor Code/Description Type", TinkarTerm.PREFERRED)
                .definition("What the Vendor Code/Description is associated with: Analyte, Target, Specimen, or Results Conformance", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.DESCRIPTION_TYPE, instEquipNameIDConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();


        //Target - can't find the target related concept we want in SNOMEDLOINC
        EntityProxy.Concept targetConcept = EntityProxy.Concept.make("Target", uuidUtility.createUUID("Target"));
        starterData.concept(targetConcept)
                .fullyQualifiedName("Target", TinkarTerm.PREFERRED)
                .synonym("Target", TinkarTerm.PREFERRED)
                .definition("The specific thing being captured/looked for that describes how the Analyte is being specifically meaured.", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, targetConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrConcept))
                .statedNavigation(List.of(targetConcept),List.of(lidrConcept))
                .build();

        /* UUID from SNOMED Browser - This is the ECL query : '57134006 |Instrument, device (physical object)|'  */
        EntityProxy.Concept instrumentIdentifier = EntityProxy.Concept.make("Device", UuidUtil.fromSNOMED("57134006"));
        starterData.concept(instrumentIdentifier)
                .fullyQualifiedName("Instrument, device (physical object)", TinkarTerm.PREFERRED)
                .synonym("Clinical instrument", TinkarTerm.PREFERRED)
                .definition("Instrument, device (physical object)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, instrumentIdentifier.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .statedNavigation(List.of(instrumentIdentifier),List.of(TinkarTerm.MODEL_CONCEPT))
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
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, testKitConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrConcept))
                .statedNavigation(List.of(testKitConcept),List.of(lidrConcept))
                .build();

        /* No UUID from SNOMED Browser -  'SNOMED has no concept for Result Conformance yet'  */
        EntityProxy.Concept resultConformanceConcept = EntityProxy.Concept.make("Result Conformance", uuidUtility.createUUID("Result Conformance"));
        starterData.concept(resultConformanceConcept)
                .fullyQualifiedName("Result Conformance", TinkarTerm.PREFERRED)
                .synonym("LIDR Result Conformance", TinkarTerm.PREFERRED)
                .definition("Laboratory Interoperability Device Reference Result Conformance", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, resultConformanceConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrConcept))
                .statedNavigation(List.of(resultConformanceConcept),List.of(lidrConcept))
                .build();

        /* No UUID from SNOMED Browser -  'SNOMED has no concept for a simple Target yet - lots of hits on word Target'  */

        /* UUID from SNOMED Browser - This is the ECL query : '246478007 |Analyte measured (attribute)|'  */
        EntityProxy.Concept analyteConcept = EntityProxy.Concept.make("Analyte", uuidUtility.createUUID("Analyte"));
        starterData.concept(analyteConcept)
                .fullyQualifiedName("Analyte", TinkarTerm.PREFERRED)
                .synonym("Analyte", TinkarTerm.PREFERRED)
                .definition("What is specifically being measured by the test", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.IDENTIFIER_SOURCE, analyteConcept.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrConcept,dataResultsType))
                .statedNavigation(List.of(dataResultsType),List.of(lidrConcept))
                .build();

        //Vendor Code/Description Pattern
        EntityProxy.Concept vendorCodeDescriptionPattern = EntityProxy.Concept.make("Vendor Code/Description Pattern", uuidUtility.createUUID("Vendor Code/Description Pattern"));
        starterData.concept(vendorCodeDescriptionPattern)
                .fullyQualifiedName("Vendor Code/Description Pattern", TinkarTerm.PREFERRED)
                .synonym("Vendor Code/Description Pattern", TinkarTerm.PREFERRED)
                .definition("A Pattern that contains semantic values that describe how a Vendor Code or Vendor Description was used to describe : Analyte(s), Target(s), Specimen(s), or Result Conformance(s)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.IDENTIFIER_SOURCE, vendorCodeDescriptionPattern.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .statedNavigation(List.of(vendorCode,vendorDescription,vendorOrDecriptionTypeConcept
                ),List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        //Reference Ranges
        EntityProxy.Concept referenceRanges = EntityProxy.Concept.make("Reference Ranges", uuidUtility.createUUID("Reference Ranges"));
        starterData.concept(referenceRanges)
                .fullyQualifiedName("Reference Ranges", TinkarTerm.PREFERRED)
                .synonym("Range of Values", TinkarTerm.PREFERRED)
                .definition("A range of normal values expected for a healthy person based on a group of otherwise healthy people", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, referenceRanges.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        //Detection Limit
        EntityProxy.Concept detectionLimit = EntityProxy.Concept.make("Detection Limit", uuidUtility.createUUID("Detection Limit"));
        starterData.concept(detectionLimit)
                .fullyQualifiedName("Detection Limit", TinkarTerm.PREFERRED)
                .synonym("Limit of Detection (LOD)", TinkarTerm.PREFERRED)
                .definition("The limit of detection LOD (or detection limit, DL) is the lowest possible concentration at which the method can detect (but not quantify!) the analyte within the matrix with certain degree of confidence.", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, detectionLimit.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();


        //Result Conformance Range Pattern
        EntityProxy.Concept resultConformanceRangePattern = EntityProxy.Concept.make("Result Conformance Range Pattern", uuidUtility.createUUID("Result Conformance Range Pattern"));
        starterData.concept(resultConformanceRangePattern)
                .fullyQualifiedName("Result Conformance Range Pattern", TinkarTerm.PREFERRED)
                .synonym("Result Conformance Range Pattern", TinkarTerm.PREFERRED)
                .definition("A Pattern that specifies the Result Ranges for the Result Conformance", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.IDENTIFIER_SOURCE, resultConformanceRangePattern.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .statedNavigation(List.of(referenceRanges),List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        //Analyte Range Pattern
        EntityProxy.Concept analyteRangePattern = EntityProxy.Concept.make("Analyte Range Pattern", uuidUtility.createUUID("Analyte Range Pattern"));
        starterData.concept(analyteRangePattern)
                .fullyQualifiedName("Analyte Range Pattern", TinkarTerm.PREFERRED)
                .synonym("Analyte Range Pattern", TinkarTerm.PREFERRED)
                .definition("A Pattern that specifies the Result Ranges and Detection Limit for the Analyte", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.IDENTIFIER_SOURCE, analyteRangePattern.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .statedNavigation(List.of(referenceRanges,detectionLimit),List.of(TinkarTerm.MODEL_CONCEPT))
                .build();


        configureLIDR_MetaDataPattern(starterData, uuidUtility);
        configurePhenomenonNavigation(starterData, uuidUtility);

    }

    private static void configureLIDR_MetaDataPattern(StarterData starterData, UUIDUtility uuidUtility) {

        EntityProxy.Concept lidrMetadataPattern = EntityProxy.Concept.make("LIDR MetaData Pattern", uuidUtility.createUUID("LIDR MetaData Pattern"));
        starterData.concept(lidrMetadataPattern)
                .fullyQualifiedName("LIDR MetaData Pattern", TinkarTerm.PREFERRED)
                .synonym("LIDR MetaData Pattern", TinkarTerm.PREFERRED)
                .definition("A Pattern that contains semantic values to describe LOINC Values assigned and reviewed by the LIDR Comittee and a Vendor", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.IDENTIFIER_SOURCE, lidrMetadataPattern.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.MODEL_CONCEPT))
                .build();

        //LOINC Version ID
        EntityProxy.Concept loincVersionID = EntityProxy.Concept.make("LOINC Version ID", uuidUtility.createUUID("LOINC Version ID"));
        starterData.concept(loincVersionID)
                .fullyQualifiedName("LOINC Version ID", TinkarTerm.PREFERRED)
                .synonym("LOINC Version ID", TinkarTerm.PREFERRED)
                .definition("The version of LOINC used to establish the LOINC mapping for the described test", TinkarTerm.SOURCE_CONTENT_VERSION)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincVersionID.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataPattern))
                .statedNavigation(List.of(loincVersionID),List.of(lidrMetadataPattern,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Date Mapping Performed
        EntityProxy.Concept dateMappingPerformed = EntityProxy.Concept.make("Date Mapping Performed", uuidUtility.createUUID("Date Mapping Performed"));
        starterData.concept(dateMappingPerformed)
                .fullyQualifiedName("Date Mapping Performed", TinkarTerm.PREFERRED)
                .synonym("Date Mapping Performed", TinkarTerm.PREFERRED)
                .definition("Date the LOINC mapping was performed - this will help understand the temporal context and if review is needed with a new LOINC release", TinkarTerm.SOURCE_RELEASE_DATE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, dateMappingPerformed.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataPattern))
                .statedNavigation(List.of(dateMappingPerformed),List.of(lidrMetadataPattern,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Source of Entry
        EntityProxy.Concept sourceOfEntry = EntityProxy.Concept.make("Source of Entry", uuidUtility.createUUID("Source of Entry"));
        starterData.concept(sourceOfEntry)
                .fullyQualifiedName("Source of Entry", TinkarTerm.PREFERRED)
                .synonym("Source of Entry", TinkarTerm.PREFERRED)
                .definition("Name of the organization that submitted the data for the LIDR Record", TinkarTerm.NAME)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, sourceOfEntry.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataPattern))
                .statedNavigation(List.of(sourceOfEntry),List.of(lidrMetadataPattern,TinkarTerm.MODEL_CONCEPT))
                .build();

        EntityProxy.Concept committeeReviewed = EntityProxy.Concept.make("Committee Reviewed", uuidUtility.createUUID("Committee Reviewed"));
        starterData.concept(committeeReviewed)
                .fullyQualifiedName("Committee Reviewed", TinkarTerm.PREFERRED)
                .synonym("Committee Reviewed", TinkarTerm.PREFERRED)
                .definition("Lists the name of the committee who reviewed the data in this row for accuracy in representing the package insert content and the LOINC (and other standard code) mapping Considered NOT REVIEWED when blank For now should only be \"LIDR\"", TinkarTerm.NAME)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, committeeReviewed.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataPattern))
                .statedNavigation(List.of(committeeReviewed),List.of(lidrMetadataPattern,TinkarTerm.MODEL_CONCEPT))
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
                .statedDefinition(List.of(lidrMetadataPattern))
                .statedNavigation(List.of(vendorReviewed),List.of(lidrMetadataPattern,TinkarTerm.MODEL_CONCEPT))
                .build();


        //Vendor Comment
        EntityProxy.Concept vendorComment = EntityProxy.Concept.make("Vendor Comment", uuidUtility.createUUID("Vendor Comment"));
        starterData.concept(vendorComment)
                .fullyQualifiedName("Vendor Comment", TinkarTerm.PREFERRED)
                .synonym("Vendor Comment", TinkarTerm.PREFERRED)
                .definition("A human-readable text clarification about the test being performed", TinkarTerm.DESCRIPTION_TYPE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, vendorComment.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataPattern))
                .statedNavigation(List.of(vendorComment),List.of(lidrMetadataPattern,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Test Ordered
        EntityProxy.Concept testOrdered = EntityProxy.Concept.make("Test Ordered", uuidUtility.createUUID("Test Ordered"));
        starterData.concept(testOrdered)
                .fullyQualifiedName("Test Ordered", TinkarTerm.PREFERRED)
                .synonym("Test Ordered analyte/observable (LOINC ®™)", TinkarTerm.PREFERRED)
                .definition("Provides the specific test that was ordered for machine interpretation and can be checked against the test that was performed.", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, testOrdered.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataPattern))
                .statedNavigation(List.of(testOrdered),List.of(lidrMetadataPattern,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Test Performed
        EntityProxy.Concept testPerformed = EntityProxy.Concept.make("Test Performed", uuidUtility.createUUID("Test Performed"));
        starterData.concept(testPerformed)
                .fullyQualifiedName("Test Performed", TinkarTerm.PREFERRED)
                .synonym("Test Performed analyte/observable (LOINC ®™)", TinkarTerm.PREFERRED)
                .definition("Provides the specific test that was performed for machine interpretation and can be checked against the test that was ordered.", TinkarTerm.IDENTIFIER_SOURCE)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, testPerformed.asUuidArray()[0].toString())
                .statedDefinition(List.of(lidrMetadataPattern))
                .statedNavigation(List.of(testPerformed),List.of(lidrMetadataPattern,TinkarTerm.MODEL_CONCEPT))
                .build();

        //LIDR MetaData Pattern
        List<EntityProxy.Concept> metaDataConcepts = Arrays.asList(TinkarTerm.MODEL_CONCEPT,
                testOrdered,
                testPerformed,dateMappingPerformed,
                loincVersionID,sourceOfEntry,
                committeeReviewed ,vendorReviewed,
                vendorComment);

        starterData.concept(lidrMetadataPattern)
                .statedNavigation(metaDataConcepts,List.of(TinkarTerm.MODEL_CONCEPT))
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
                .synonym("Component", TinkarTerm.PREFERRED)
                .definition("The substance or entity being measured or observed", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincComponent.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(loincComponent),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Property   From SNOMED-LOINC : 370130000 |Property (attribute)|
        EntityProxy.Concept loincProperty = EntityProxy.Concept.make("Property", UuidUtil.fromSNOMED("370130000"));
        starterData.concept(loincProperty)
                .fullyQualifiedName("Property (attribute)", TinkarTerm.PREFERRED)
                .synonym("Property", TinkarTerm.PREFERRED)
                .definition("The characteristic or attribute of the analyte, distinguishes between different kinds of quantities relating to the same substance, e.g. Mass versus Counts vs time duration", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincProperty.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(loincProperty),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Time Aspect   From SNOMED-LOINC : 370134009 |Time aspect (attribute)|
        EntityProxy.Concept loincTimeAspect = EntityProxy.Concept.make("Time aspect", UuidUtil.fromSNOMED("370134009"));
        starterData.concept(loincTimeAspect)
                .fullyQualifiedName("Time aspect (attribute)", TinkarTerm.PREFERRED)
                .synonym("Time aspect", TinkarTerm.PREFERRED)
                .definition("One can either measure a Property at a moment (point) in time or measure it over a time interval and integrate, in the mathematical sense, over time.The interval of time over which an observation was made. (e.g., Point in time vs 24 hrs collection)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincTimeAspect.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(loincTimeAspect),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Direct site   From SNOMED-LOINC : 704327008 |Direct site (attribute)|
        EntityProxy.Concept loincDirectSite = EntityProxy.Concept.make("Direct site", UuidUtil.fromSNOMED("704327008"));
        starterData.concept(loincDirectSite)
                .fullyQualifiedName("Direct site (attribute)", TinkarTerm.PREFERRED)
                .synonym("Direct site", TinkarTerm.PREFERRED)
                .definition("Refers to the specific anatomical location or site where a particular observation or test is performed. ", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincDirectSite.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(loincDirectSite),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Scale type   From SNOMED-LOINC : 370132008 |Scale type (attribute)|
        EntityProxy.Concept loincScaleType = EntityProxy.Concept.make("Scale type", UuidUtil.fromSNOMED("370132008"));
        starterData.concept(loincScaleType)
                .fullyQualifiedName("Scale type (attribute)", TinkarTerm.PREFERRED)
                .synonym("Scale type", TinkarTerm.PREFERRED)
                .definition("How the observation value is quantified or expressed. Type of scale specifies the scale of the measure. The following scale types are defined: Quantitative (Qn), Ordinal (Ord), Nominal (Nom), Narrative (Nar)", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincScaleType.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(loincScaleType),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Method From SNOMED-LOINC :  From SNOMED-LOINC : 260686004 |Method (attribute)|
        EntityProxy.Concept methodType = EntityProxy.Concept.make("Method", UuidUtil.fromSNOMED("260686004"));
        starterData.concept(methodType)
                .fullyQualifiedName("Method (attribute)", TinkarTerm.PREFERRED)
                .synonym("Method", TinkarTerm.PREFERRED)
                .definition("A high-level classification of how the observation was made. What methodology is being used to make the measurement. Only needed when the technique affects the clinical interpretation of the results.", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, methodType.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(methodType),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        //Units from SNOMED-LOINC :  From SNOMED-LOINC : 246514001 |Units (attribute)|
        EntityProxy.Concept loincUnitsType = EntityProxy.Concept.make("Units", UuidUtil.fromSNOMED("246514001"));
        starterData.concept(loincUnitsType)
                .fullyQualifiedName("Units (attribute)", TinkarTerm.PREFERRED)
                .synonym("Units", TinkarTerm.PREFERRED)
                .definition("Provides expected UCUM (Unified Code for Units of Measure) units of measure for  values of an observation", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, loincUnitsType.asUuidArray()[0].toString())
                .statedDefinition(List.of(phenomenonConcept))
                .statedNavigation(List.of(loincUnitsType),List.of(phenomenonConcept,TinkarTerm.MODEL_CONCEPT))
                .build();

        lidrPhenomenonConcepts.addAll(List.of(loincComponent,loincProperty,loincTimeAspect,
                loincDirectSite, loincScaleType, methodType,loincUnitsType));

        starterData.concept(phenomenonConcept)
                .statedNavigation(lidrPhenomenonConcepts,List.of(TinkarTerm.MODEL_CONCEPT))
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
