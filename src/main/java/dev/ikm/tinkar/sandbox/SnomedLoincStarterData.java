package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.service.CachingService;
import dev.ikm.tinkar.common.service.PrimitiveData;
import dev.ikm.tinkar.common.service.ServiceKeys;
import dev.ikm.tinkar.common.service.ServiceProperties;
import dev.ikm.tinkar.common.util.uuid.UuidUtil;
import dev.ikm.tinkar.composer.Composer;
import dev.ikm.tinkar.composer.Session;
import dev.ikm.tinkar.composer.assembler.ConceptAssembler;
import dev.ikm.tinkar.composer.template.Definition;
import dev.ikm.tinkar.composer.template.FullyQualifiedName;
import dev.ikm.tinkar.composer.template.Identifier;
import dev.ikm.tinkar.composer.template.StatedAxiom;
import dev.ikm.tinkar.composer.template.StatedNavigation;
import dev.ikm.tinkar.composer.template.Synonym;
import dev.ikm.tinkar.composer.template.TinkarBaseModel;
import dev.ikm.tinkar.composer.template.USDialect;
import dev.ikm.tinkar.entity.EntityService;
import dev.ikm.tinkar.entity.export.ExportEntitiesController;
import dev.ikm.tinkar.starterdata.UUIDUtility;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.State;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.concurrent.ExecutionException;

import static dev.ikm.tinkar.terms.TinkarTerm.DESCRIPTION_NOT_CASE_SENSITIVE;
import static dev.ikm.tinkar.terms.TinkarTerm.ENGLISH_LANGUAGE;
import static dev.ikm.tinkar.terms.TinkarTerm.IDENTIFIER_SOURCE;
import static dev.ikm.tinkar.terms.TinkarTerm.PREFERRED;
import static dev.ikm.tinkar.terms.TinkarTerm.PRIMORDIAL_MODULE;
import static dev.ikm.tinkar.terms.TinkarTerm.PRIMORDIAL_PATH;
import static dev.ikm.tinkar.terms.TinkarTerm.UNIVERSALLY_UNIQUE_IDENTIFIER;
import static dev.ikm.tinkar.terms.TinkarTerm.USER;

public class SnomedLoincStarterData {

    private static final Logger LOG = LoggerFactory.getLogger(SnomedLoincStarterData.class.getSimpleName());

    private final File datastore;
    private final File exportFile;

    public SnomedLoincStarterData(String[] args) {
        datastore = new File(args[0]);
        exportFile = new File(args[1]);
    }

    private void transform() {
        EntityService.get().beginLoadPhase();
        try {
            Composer composer = new Composer("SnomedCT Loinc Starter Data");
            Session session = composer.open(
                    State.ACTIVE,
                    System.currentTimeMillis(),
                    USER,
                    PRIMORDIAL_MODULE,
                    PRIMORDIAL_PATH);
            createConcepts(session);
            composer.commitSession(session);
        } finally {
            EntityService.get().endLoadPhase();
        }
    }

    private void createConcepts(Session session) {
        EntityProxy.Concept snomedLoincAuthor = EntityProxy.Concept.make("SNOMED CT LOINC Collaboration Author", new UUIDUtility().createUUID("SNOMED CT LOINC Collaboration Author"));

        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(snomedLoincAuthor))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text("IHTSDO SNOMED CT LOINC Collaboration Author")
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text(snomedLoincAuthor.description())
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("International Health Terminology Standards Development Organisation (IHTSDO) SNOMED CT LOINC Collaboration Author")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(snomedLoincAuthor.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(USER))
                .attach(new StatedAxiom()
                        .isA(USER)).attach(new TinkarBaseModel());

        EntityProxy.Concept loincIdentifier = EntityProxy.Concept.make("LOINC Number", UuidUtil.fromSNOMED("705114005"));
        session.compose((ConceptAssembler conceptAssembler) -> conceptAssembler.concept(loincIdentifier))
                .attach((FullyQualifiedName fqn) -> fqn
                        .text(loincIdentifier.description())
                        .language(ENGLISH_LANGUAGE)
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .attach(usDialect()))
                .attach((Synonym synonym) -> synonym
                        .text("LOINC Num")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Definition definition) -> definition
                        .text("Unique point of origin for identifier")
                        .caseSignificance(DESCRIPTION_NOT_CASE_SENSITIVE)
                        .language(ENGLISH_LANGUAGE)
                        .attach(usDialect()))
                .attach((Identifier identifier) -> identifier
                        .source(UNIVERSALLY_UNIQUE_IDENTIFIER)
                        .identifier(loincIdentifier.asUuidArray()[0].toString()))
                .attach(new StatedNavigation()
                        .parents(IDENTIFIER_SOURCE))
                .attach(new StatedAxiom()
                        .isA(IDENTIFIER_SOURCE)).attach(new TinkarBaseModel());
    }

    private USDialect usDialect() {
        return new USDialect().acceptability(PREFERRED);
    }

    private void init() {
        LOG.info("Starting database");
        LOG.info("Loading data from " + datastore.getAbsolutePath());
        CachingService.clearAll();
        ServiceProperties.set(ServiceKeys.DATA_STORE_ROOT, datastore);
        PrimitiveData.selectControllerByName("Open SpinedArrayStore");
        PrimitiveData.start();
    }

    private void cleanup() {
        PrimitiveData.stop();
    }

    private void exportToProtoBuf() {
        try {
            new ExportEntitiesController().export(exportFile).get();
        } catch (ExecutionException | InterruptedException e) {
            LOG.error("Error while exporting.", e);
        }
    }

    public static void main(String[] args) {
        SnomedLoincStarterData starterData = new SnomedLoincStarterData(args);
        starterData.init();
        starterData.transform();
        starterData.exportToProtoBuf();
        starterData.cleanup();
    }
}
