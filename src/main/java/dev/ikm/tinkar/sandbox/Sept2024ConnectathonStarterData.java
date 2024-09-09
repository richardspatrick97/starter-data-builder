package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.starterdata.StarterData;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.EntityProxy.Concept;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutionException;

public class Sept2024ConnectathonStarterData {
    private static File exportFile;

    public static void main(String[] args){

        TinkarStarterData.main(args);
        File exportDataStore = new File(args[0]);
        exportFile = new File(args[1]);
        UUIDUtility uuidUtility = new UUIDUtility();

        //Build, export, and shutdown database
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
        exportStarterData();  //exports starter data to pb.zip
        starterData.shutdown();
    }

    protected static void configureConceptsAndPatterns(StarterData starterData, UUIDUtility uuidUtility){
        Concept snomedAuthor = Concept.make("IHTSDO SNOMED CT Author", uuidUtility.createUUID("IHTSDO SNOMED CT Author"));
        starterData.concept(snomedAuthor)
                .fullyQualifiedName("IHTSDO SNOMED CT Author", TinkarTerm.PREFERRED)
                .synonym("SNOMED CT Author", TinkarTerm.PREFERRED)
                .definition("International Health Terminology Standards Development Organisation (IHTSDO) SNOMED CT Author", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedAuthor.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        Concept snomedIdentifier = Concept.make("SNOMED CT Identifier", UuidUtil.fromSNOMED("900000000000294009"));
        starterData.concept(snomedIdentifier)
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        configureConnectathonPatterns( starterData,  uuidUtility);

    }

    protected static void configureValueContraintSemantics(StarterData starterData, UUIDUtility uuidUtility){
        Concept snomedAuthor = Concept.make("IHTSDO SNOMED CT Author", uuidUtility.createUUID("IHTSDO SNOMED CT Author"));
        starterData.concept(snomedAuthor)
                .fullyQualifiedName("IHTSDO SNOMED CT Author", TinkarTerm.PREFERRED)
                .synonym("SNOMED CT Author", TinkarTerm.PREFERRED)
                .definition("International Health Terminology Standards Development Organisation (IHTSDO) SNOMED CT Author", TinkarTerm.PREFERRED)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, snomedAuthor.asUuidArray()[0].toString())
                .statedDefinition(List.of(TinkarTerm.USER))
                .build();

        Concept snomedIdentifier = Concept.make("SNOMED CT Identifier", UuidUtil.fromSNOMED("900000000000294009"));
        starterData.concept(snomedIdentifier)
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

    }

    protected static void configureConnectathonPatterns(StarterData starterData, UUIDUtility uuidUtility){

        createPresenceOfCovidPattern(starterData, uuidUtility);

        createAbsenceOfCovidPattern(starterData, uuidUtility);

        createUndeterminedCovidTestResultPattern(starterData, uuidUtility);

        createInvalidCovidTestResultPattern(starterData, uuidUtility);


    }

    private static void createInvalidCovidTestResultPattern(StarterData starterData, UUIDUtility uuidUtility) {
        Concept invalid = Concept.make("Undetermined", UuidUtil.fromSNOMED("455371000124106"));
        starterData.concept(invalid)
                .build();

        starterData.pattern( EntityProxy.Pattern.make("Invalid Covid Test Result Pattern", uuidUtility.createUUID("Invalid Covid Test Result Pattern")))
                .meaning(invalid)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();
    }

    private static void createUndeterminedCovidTestResultPattern(StarterData starterData, UUIDUtility uuidUtility) {
        Concept undetermined = Concept.make("Undetermined", UuidUtil.fromSNOMED("373068000"));
        starterData.concept(undetermined)
                .build();

        starterData.pattern( EntityProxy.Pattern.make("Undetermined Covid Test Result Pattern", uuidUtility.createUUID("Undetermined Covid Test Result Pattern")))
                .meaning(undetermined)
                .purpose(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .build();
    }

    private static void createAbsenceOfCovidPattern(StarterData starterData, UUIDUtility uuidUtility) {
        Concept absenceFindings = Concept.make("Abscence findings", UuidUtil.fromSNOMED("272519000"));
        starterData.concept(absenceFindings)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, absenceFindings.asUuidArray()[0].toString())
                .build();

        Concept absent = Concept.make("Absent", UuidUtil.fromSNOMED("2667000"));
        starterData.concept(absent)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, absent.asUuidArray()[0].toString())
                .build();

        Concept negative = Concept.make("Negative", UuidUtil.fromSNOMED("260385009"));
        starterData.concept(negative)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, negative.asUuidArray()[0].toString())
                .build();

        Concept notDetected = Concept.make("Not Detected", UuidUtil.fromSNOMED("260415000"));
        starterData.concept(notDetected)
                .identifier(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER, notDetected.asUuidArray()[0].toString())
                .build();

//        List<Concept> resultConcepts = Arrays.asList(absent, negative,  notDetected);
//
//        starterData.concept(absenceFindings)
//                .statedDefinition(List.of(absenceFindings))
//                .statedNavigation(resultConcepts, List.of(absenceFindings))
//                .build();

        starterData.pattern( EntityProxy.Pattern.make("Absence Of Covid Pattern", uuidUtility.createUUID("Absence Of Covid Pattern")))
                .meaning(absenceFindings)
                .purpose(TinkarTerm.MEANING)
                .build();
    }

    private static void createPresenceOfCovidPattern(StarterData starterData, UUIDUtility uuidUtility) {
        Concept presenceFindings = Concept.make("Presence findings", UuidUtil.fromSNOMED("260411009"));
        starterData.concept(presenceFindings)
                .build();

        Concept present = Concept.make("Present", UuidUtil.fromSNOMED("52101004"));
        starterData.concept(present)
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        Concept detected = Concept.make("Detected", UuidUtil.fromSNOMED("260373001"));
        starterData.concept(detected)
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        Concept positive = Concept.make("Positive", UuidUtil.fromSNOMED("10828004"));
        starterData.concept(positive)
                .statedDefinition(List.of(TinkarTerm.IDENTIFIER_SOURCE))
                .build();

        Concept presumptivePositive = Concept.make("Presumptive Positive", UuidUtil.fromSNOMED("720735008"));
        starterData.concept(presumptivePositive)
                .statedDefinition(List.of(presenceFindings))
                .build();

        List<Concept> resultConcepts = Arrays.asList(present, detected,  positive, presumptivePositive);

        starterData.concept(presenceFindings)
                .statedDefinition(List.of(presenceFindings))
                .statedNavigation(resultConcepts, List.of(presenceFindings))
                .build();

        starterData.pattern( EntityProxy.Pattern.make("Presence of Covid Pattern", uuidUtility.createUUID("Presence of Covid Pattern")))
                .meaning(TinkarTerm.MEMBERSHIP_SEMANTIC)
                .purpose(TinkarTerm.MEANING)
                .fieldDefinition(
                        presenceFindings,
                        presenceFindings,
                        TinkarTerm.CONCEPT_TYPE)
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
