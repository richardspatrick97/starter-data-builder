package dev.ikm.tinkar.starterdata;

import dev.ikm.tinkar.common.service.CachingService;
import dev.ikm.tinkar.common.service.PrimitiveData;
import dev.ikm.tinkar.common.service.ServiceKeys;
import dev.ikm.tinkar.common.service.ServiceProperties;
import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.composer.Composer;
import dev.ikm.tinkar.composer.Session;
import dev.ikm.tinkar.composer.assembler.ConceptAssembler;
import dev.ikm.tinkar.composer.template.*;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.State;
import dev.ikm.tinkar.terms.TinkarTerm;

import java.io.File;

import static dev.ikm.tinkar.terms.TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE;
import static dev.ikm.tinkar.terms.TinkarTerm.ENGLISH_LANGUAGE;
public class SnomedStarterDataComposer {
    public static void main(String[] args) {
        new SnomedStarterDataComposer().buildSnomedStarterData();
    }

    private void buildSnomedStarterData() {
        File datastore = new File("/Users/jsteinhafel/Solor/generated-data");

        CachingService.clearAll();
        ServiceProperties.set(ServiceKeys.DATA_STORE_ROOT, datastore);
        PrimitiveData.selectControllerByName("Open SpinedArrayStore");
        PrimitiveData.start();

        Composer composer = new Composer("Snomed Starter Data Composer");

        UUIDUtility uuidUtility = new UUIDUtility();

        Session session = composer.open(State.ACTIVE,
                PrimitiveData.PREMUNDANE_TIME,
                TinkarTerm.USER,
                TinkarTerm.PRIMORDIAL_MODULE,
                TinkarTerm.PRIMORDIAL_PATH);

        EntityProxy.Concept snomedAuthor = EntityProxy.Concept.make("IHTSDO SNOMED CT Author", uuidUtility.createUUID("IHTSDO SNOMED CT Author"));
        session.compose((ConceptAssembler concept) -> concept
                .concept(snomedAuthor)
                .attach((FullyQualifiedName fqn) -> fqn
                        .language(ENGLISH_LANGUAGE)
                        .text("IHTSDO SNOMED CT Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                )
                .attach((Synonym synonym)-> synonym
                        .language(ENGLISH_LANGUAGE)
                        .text("SNOMED CT Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                )
                .attach((Definition definition) -> definition
                        .language(ENGLISH_LANGUAGE)
                        .text("International Health Terminology Standards Development Organisation (IHTSDO) SNOMED CT Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                )
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(snomedAuthor.asUuidArray()[0].toString())
                )
                .attach((StatedAxiom statedAxiom) -> statedAxiom
                        .isA(TinkarTerm.USER)
                )
        );

        EntityProxy.Concept snomedIdentifier = EntityProxy.Concept.make("SNOMED CT Identifier", UuidUtil.fromSNOMED("900000000000294009"));
        session.compose((ConceptAssembler concept) -> concept
                .concept(snomedIdentifier)
                .attach((FullyQualifiedName fqn) -> fqn
                        .language(ENGLISH_LANGUAGE)
                        .text("SNOMED CT Identifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                )
                .attach((Synonym synonym)-> synonym
                        .language(ENGLISH_LANGUAGE)
                        .text("SCTID")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                )
                .attach((Definition definition) -> definition
                        .language(ENGLISH_LANGUAGE)
                        .text("Unique point of origin for identifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                )
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(snomedIdentifier.asUuidArray()[0].toString())
                )
                .attach((StatedAxiom statedAxiom) -> statedAxiom
                        .isA(TinkarTerm.IDENTIFIER_SOURCE)
                )
        );

        composer.commitSession(session);
    }

    private void createConcepts(Session session, UUIDUtility uuidUtility) {

        EntityProxy.Concept snomedAuthor = EntityProxy.Concept.make("IHTSDO SNOMED CT Author", uuidUtility.createUUID("IHTSDO SNOMED CT Author"));
        session.compose((ConceptAssembler concept) -> concept
                .concept(snomedAuthor)
                .attach((FullyQualifiedName fqn) -> fqn
                        .language(ENGLISH_LANGUAGE)
                        .text("IHTSDO SNOMED CT Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                )
                .attach((Synonym synonym)-> synonym
                        .language(ENGLISH_LANGUAGE)
                        .text("SNOMED CT Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                )
                .attach((Definition definition) -> definition
                        .language(ENGLISH_LANGUAGE)
                        .text("International Health Terminology Standards Development Organisation (IHTSDO) SNOMED CT Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                )
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(snomedAuthor.asUuidArray()[0].toString())
                )
                .attach((StatedAxiom statedAxiom) -> statedAxiom
                        .isA(TinkarTerm.USER)
                )
        );

        EntityProxy.Concept  snomedIdentifier = EntityProxy.Concept.make("SNOMED CT Identifier", UuidUtil.fromSNOMED("900000000000294009"));
        session.compose((ConceptAssembler concept) -> concept
                .concept(snomedIdentifier)
                .attach((FullyQualifiedName fqn) -> fqn
                        .language(ENGLISH_LANGUAGE)
                        .text("SNOMED CT Identifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                )
                .attach((Synonym synonym)-> synonym
                        .language(ENGLISH_LANGUAGE)
                        .text("SCTID")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                )
                .attach((Definition definition) -> definition
                        .language(ENGLISH_LANGUAGE)
                        .text("Unique point of origin for identifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                )
                .attach((Identifier identifier) -> identifier
                        .source(TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(snomedIdentifier.asUuidArray()[0].toString())
                )
                .attach((StatedAxiom statedAxiom) -> statedAxiom
                        .isA(TinkarTerm.IDENTIFIER_SOURCE)
                )
        );

    }

}
