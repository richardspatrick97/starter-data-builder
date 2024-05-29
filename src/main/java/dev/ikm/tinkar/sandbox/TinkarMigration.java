package dev.ikm.tinkar.sandbox;

import dev.ikm.tinkar.common.service.CachingService;
import dev.ikm.tinkar.common.service.PrimitiveData;
import dev.ikm.tinkar.common.service.ServiceKeys;
import dev.ikm.tinkar.common.service.ServiceProperties;
import dev.ikm.tinkar.common.util.io.FileUtil;
import dev.ikm.tinkar.component.FieldDataType;
import dev.ikm.tinkar.entity.*;
import dev.ikm.tinkar.terms.ComponentWithNid;
import dev.ikm.tinkar.terms.TinkarTerm;
import org.eclipse.collections.api.block.procedure.primitive.IntProcedure;
import org.eclipse.collections.api.list.ImmutableList;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.util.stream.Collectors;

public class TinkarMigration {

    private final Logger LOG = LoggerFactory.getLogger(TinkarMigration.class.getSimpleName());

    private final File sourceDatastore = new File(System.getProperty("user.home") + "/Solor/solor-ids-256.sa");
    private final File targetDatastore = new File(System.getProperty("user.home") + "/Solor/starter-data");
    private final List<Integer> stampNids = new ArrayList<>();
    private final List<Entity<? extends EntityVersion>> entitiesToMigrate = new ArrayList<>();
    private CompletableFuture<List<Entity<? extends EntityVersion>>> extractConceptsFuture;
    private CompletableFuture<List<Entity<? extends EntityVersion>>> extractSemanticsFuture;
    private CompletableFuture<List<Entity<? extends EntityVersion>>> extractPatternsFuture;


    public static void main(String[] args){
        TinkarMigration tinkarMigration = new TinkarMigration();
        tinkarMigration.performMigration();
    }


    public void performMigration() {
        LOG.info("Start Migration");

        openDatabase(sourceDatastore);

        //Extract wanted STAMP Entities
        List<Entity<? extends EntityVersion>> stampEntities = extractStamps();
        stampNids.addAll(stampEntities.stream().map(ComponentWithNid::nid).toList());
        logCompletion(stampEntities, "STAMP");

        //Extract all Entities that are versioned wanted STAMPs
        extractConceptsFuture = CompletableFuture.supplyAsync(() -> {
            List<Entity<? extends EntityVersion>> conceptEntities = new ArrayList<>();
            PrimitiveData.get().forEachConceptNid(extractEntitiesProcedure(conceptEntities, "Concept"));
            logCompletion(conceptEntities, "Concept");
            return  conceptEntities;
        });

        extractSemanticsFuture = CompletableFuture.supplyAsync(() -> {
            List<Entity<? extends EntityVersion>> semanticEntities = new ArrayList<>();
            PrimitiveData.get().forEachSemanticNid(extractEntitiesProcedure(semanticEntities, "Semantic"));
            logCompletion(semanticEntities, "Semantic");
            return semanticEntities;
        });

        extractPatternsFuture = CompletableFuture.supplyAsync(() -> {
           List<Entity<? extends EntityVersion>> patternEntities = new ArrayList<>();
           PrimitiveData.get().forEachPatternNid(extractEntitiesProcedure(patternEntities, "Pattern"));
            logCompletion(patternEntities, "Pattern");
            return patternEntities;
        });

        //Aggregate all Entities extracted
        CompletableFuture.allOf(extractConceptsFuture, extractSemanticsFuture, extractPatternsFuture)
                .whenComplete((unused, throwable) -> {
                    entitiesToMigrate.addAll(stampEntities);
                    entitiesToMigrate.addAll(extractConceptsFuture.join());
                    entitiesToMigrate.addAll(extractSemanticsFuture.join());
                    entitiesToMigrate.addAll(extractPatternsFuture.join());
                    logCompletion(entitiesToMigrate, "All");
                })
                .join();

        List<Entity<? extends EntityVersion>> entities = entitiesToMigrate.stream()
                        .filter(entity -> entity.entityDataType() == FieldDataType.SEMANTIC_CHRONOLOGY)
                                .filter(entity -> ((SemanticRecord)entity).patternNid() == TinkarTerm.EL_PLUS_PLUS_STATED_AXIOMS_PATTERN.nid())
                                        .collect(Collectors.toList());


        stopDatabase();
        FileUtil.recursiveDelete(targetDatastore);
        openDatabase(targetDatastore);
        entitiesToMigrate.forEach(entity -> EntityService.get().putEntity(entity));
        stopDatabase();

        LOG.info("Finish Migration");
    }

    private List<Entity<? extends EntityVersion>> extractStamps() {
        LOG.info("Started Reading STAMP Entities");
        List<Entity<? extends EntityVersion>> stampEntities = new ArrayList<>();
        PrimitiveData.get().forEachStampNid(stampNid -> {
            int moduleNid = Entity.getStamp(stampNid).moduleNid();
            switch (PrimitiveData.text(moduleNid)) {
                case "Primordial module", "Uninitialized", "KOMET module", "users module" -> stampEntities.add(Entity.getFast(stampNid));
            }
        });
        return stampEntities;
    }

    private IntProcedure extractEntitiesProcedure(List<Entity<? extends EntityVersion>> entities, String entityType){
        return  entityNid -> {
            final Entity<EntityVersion> entity = Entity.getFast(entityNid);
            final List<Integer> applicableSTAMPNids = new ArrayList<>();
            final List<Integer> nonApplicableSTAMPNids = new ArrayList<>();
            final AtomicBoolean isMigratable = new AtomicBoolean(false);
            entity.stampNids().forEach(nid -> {
                if (stampNids.contains(nid)) {
                    applicableSTAMPNids.add(nid);
                    if (!entities.contains(entity)) {
                        isMigratable.set(true);
                    }
                } else {
                    nonApplicableSTAMPNids.add(nid);
                }
            });
            if (isMigratable.get()){
                filter(entities, entity, applicableSTAMPNids, nonApplicableSTAMPNids);
                logProgress(entities, entityType);
            }
        };
    }

    private void debug(Entity<EntityVersion> entity){
        UUID uuid = UUID.fromString("46ae9325-dd24-5008-8fda-80cf1f0977c7");
        int roleId = -2141148457;

        if(entity.publicId().contains(uuid)){
            LOG.info(entity.toString());
        }
        if (entity.entityDataType() == FieldDataType.SEMANTIC_CHRONOLOGY){
            SemanticEntity semanticEntity = (SemanticEntity) entity;
            if(semanticEntity.referencedComponent().publicId().contains(uuid)){
                LOG.info(semanticEntity.toString());
            }
            if(semanticEntity.patternNid() == TinkarTerm.DESCRIPTION_PATTERN.nid()){
            }
        }
    }

    private void filter(List<Entity<? extends EntityVersion>> entities, Entity<EntityVersion> entity, List<Integer> applicableSTAMPNids, List<Integer> nonApplicableSTAMPNids){
        if (nonApplicableSTAMPNids.size() == 0){
            entities.add(entity);
        } else if (nonApplicableSTAMPNids.size() > 0){
            switch (entity.entityDataType()){
                case CONCEPT_CHRONOLOGY -> entities.add(filteredConceptEntity(entity, applicableSTAMPNids));
                case SEMANTIC_CHRONOLOGY -> entities.add(filteredSemanticEntity(entity, applicableSTAMPNids));
                case PATTERN_CHRONOLOGY -> entities.add(filteredPatternEntity(entity, applicableSTAMPNids));
            }
        }
    }

    private Entity<? extends EntityVersion> filteredConceptEntity(Entity<EntityVersion> originalEntity, List<Integer> stampNids){
        ConceptEntity conceptEntity = (ConceptEntity) EntityService.get().getEntityFast(originalEntity.nid());
        RecordListBuilder<ConceptVersionRecord> versions = RecordListBuilder.make();
        ConceptRecord conceptRecord = ConceptRecordBuilder.builder()
                .nid(conceptEntity.nid())
                .leastSignificantBits(conceptEntity.leastSignificantBits())
                .mostSignificantBits(conceptEntity.mostSignificantBits())
                .additionalUuidLongs(conceptEntity.additionalUuidLongs())
                .versions(versions.toImmutable())
                .build();

        stampNids.forEach(stampNid -> versions.add(ConceptVersionRecordBuilder.builder()
                .chronology(conceptRecord)
                .stampNid(stampNid)
                .build()));

        return ConceptRecordBuilder.builder(conceptRecord).versions(versions.toImmutable()).build();
    }

    private Entity<? extends EntityVersion> filteredSemanticEntity(Entity<EntityVersion> originalEntity, List<Integer> stampNids){
        SemanticEntity semanticEntity = (SemanticEntity) originalEntity;
        RecordListBuilder<SemanticVersionRecord> versions = RecordListBuilder.make();
        SemanticRecord semanticRecord = SemanticRecordBuilder.builder()
                .nid(originalEntity.nid())
                .leastSignificantBits(semanticEntity.leastSignificantBits())
                .mostSignificantBits(semanticEntity.mostSignificantBits())
                .additionalUuidLongs(semanticEntity.additionalUuidLongs())
                .patternNid(semanticEntity.patternNid())
                .referencedComponentNid(semanticEntity.referencedComponentNid())
                .versions(versions.toImmutable())
                .build();

        stampNids.forEach(stampNid -> {
            SemanticEntityVersion semanticEntityVersion = (SemanticEntityVersion) semanticEntity.getVersionFast(stampNid);
            versions.add(SemanticVersionRecordBuilder.builder()
                    .chronology(semanticRecord)
                    .stampNid(stampNid)
                    .fieldValues(semanticEntityVersion.fieldValues())
                    .build());
        });

        return SemanticRecordBuilder.builder(semanticRecord).versions(versions.toImmutable()).build();
    }

    private Entity<? extends EntityVersion> filteredPatternEntity(Entity<EntityVersion> originalEntity, List<Integer> stampNids){
        PatternEntity patternEntity = (PatternEntity) originalEntity;
        RecordListBuilder<PatternVersionRecord> versions = RecordListBuilder.make();
        PatternRecord patternRecord = PatternRecordBuilder.builder()
                .nid(patternEntity.nid())
                .leastSignificantBits(patternEntity.leastSignificantBits())
                .mostSignificantBits(patternEntity.mostSignificantBits())
                .additionalUuidLongs(patternEntity.additionalUuidLongs())
                .versions(versions.toImmutable())
                .build();

        stampNids.forEach(stampNid -> {
            PatternEntityVersion patternEntityVersion = (PatternEntityVersion) patternEntity.getVersionFast(stampNid);
            versions.add(PatternVersionRecordBuilder.builder()
                    .chronology(patternRecord)
                    .stampNid(stampNid)
                    .semanticMeaningNid(patternEntityVersion.semanticMeaningNid())
                    .semanticPurposeNid(patternEntityVersion.semanticPurposeNid())
                    .fieldDefinitions((ImmutableList<FieldDefinitionRecord>) patternEntityVersion.fieldDefinitions())
                    .build());
        });

        return PatternRecordBuilder.builder(patternRecord).versions(versions.toImmutable()).build();
    }

    private void openDatabase(File datastore) {
        LOG.info("Starting database");
        LOG.info("Loading data from " + datastore.getAbsolutePath());
        CachingService.clearAll();
        ServiceProperties.set(ServiceKeys.DATA_STORE_ROOT, datastore);
        PrimitiveData.selectControllerByName("Open SpinedArrayStore");
        PrimitiveData.start();
    }

    private void stopDatabase() {
        LOG.info("Stopping database");
        PrimitiveData.stop();
    }

    private void logProgress(List<Entity<? extends EntityVersion>> entities, String entityType){
        if (entities.size() % 1000 == 0) {
            LOG.info(entities.size() + " " + entityType + " Entities extracted");
        }
    }

    private void logCompletion(List<Entity<? extends EntityVersion>> entities, String entityType){
        LOG.info("Finish " + entityType + " Entities Extract: " + entities.size());
    }
}
