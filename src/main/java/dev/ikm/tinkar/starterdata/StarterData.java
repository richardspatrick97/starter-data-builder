package dev.ikm.tinkar.starterdata;

import dev.ikm.tinkar.common.service.CachingService;
import dev.ikm.tinkar.common.service.PrimitiveData;
import dev.ikm.tinkar.common.service.ServiceKeys;
import dev.ikm.tinkar.common.service.ServiceProperties;
import dev.ikm.tinkar.entity.*;
import dev.ikm.tinkar.terms.EntityProxy;
import dev.ikm.tinkar.terms.TinkarTerm;
import org.eclipse.collections.api.factory.Lists;
import org.eclipse.collections.api.list.ImmutableList;
import org.eclipse.collections.api.list.MutableList;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.function.Function;
import java.util.logging.Logger;

public class StarterData {

    private static final Logger LOG = Logger.getLogger(StarterData.class.getSimpleName());

    private final File datastore;
    private Entity<? extends EntityVersion> authoringSTAMP;
    private final List<Entity<? extends EntityVersion>> starterDataEntities;

    private final UUIDUtility uuidUtility;
    private final STAMPUtility stampUtility;
    private final ConceptUtility conceptUtility;
    private final PatternUtility patternUtility;
    private final SemanticUtility semanticUtility;

    //TODO-aks8m: These Patterns below need to be in Tinkar Bindings
    public static final EntityProxy.Pattern identifierPattern = EntityProxy.Pattern.make("Identifier Pattern", UUID.fromString("5d60e14b-c410-5172-9559-3c4253278ae2"));
    public static final EntityProxy.Pattern axiomSyntaxPattern = EntityProxy.Pattern.make("Axiom Syntax Pattern", UUID.fromString("c0ca180b-aae2-5fa1-9ab7-4a24f2dfe16b"));
    public static final EntityProxy.Pattern pathMembershipPattern = EntityProxy.Pattern.make("Path membership", UUID.fromString("1eb187f9-9844-55de-b575-9487ea0b2c61"));
    public static final EntityProxy.Pattern versionControlPathOriginPattern = EntityProxy.Pattern.make("Version Control Pattern", UUID.fromString("cad7b375-e4c0-5fd8-a346-63b8718f74a3"));


    public StarterData(File datastore, UUIDUtility uuidUtility) {
        this.datastore = datastore;
        this.starterDataEntities = new ArrayList<>();

        this.uuidUtility = uuidUtility;
        this.stampUtility = new STAMPUtility(this.uuidUtility);
        this.conceptUtility = new ConceptUtility(this.uuidUtility);
        this.patternUtility = new PatternUtility(this.uuidUtility);
        this.semanticUtility = new SemanticUtility(this.uuidUtility);
    }

    public StarterData init(){
        LOG.info("Starting database");
        LOG.info("Loading data from " + datastore.getAbsolutePath());
        CachingService.clearAll();
        ServiceProperties.set(ServiceKeys.DATA_STORE_ROOT, datastore);
        PrimitiveData.selectControllerByName("Open SpinedArrayStore");
        PrimitiveData.start();
        return this;
    }

    public StarterData visualize(){
        return this;
    }

    public StarterData validate(){
        return this;
    }

    public StarterData build() {
        starterDataEntities.forEach(entity -> EntityService.get().putEntity(entity));

        StringBuilder sb = new StringBuilder();

        sb.append("STAMPs: " + starterDataEntities.stream().filter(entity -> entity instanceof StampRecord).count() + "\n");
        sb.append("Concepts: " + starterDataEntities.stream().filter(entity -> entity instanceof ConceptRecord).count() + "\n");
        sb.append("Semantics: " + starterDataEntities.stream().filter(entity -> entity instanceof SemanticRecord).count() + "\n");
        sb.append("Patterns: " + starterDataEntities.stream().filter(entity -> entity instanceof PatternRecord).count());

        LOG.info(sb.toString());

        starterDataEntities.clear();
        return this;
    }

    public StarterData shutdown(){
        PrimitiveData.stop();
        return this;
    }

    public StarterData authoringSTAMP(EntityProxy.Concept status, long time, EntityProxy.Concept author, EntityProxy.Concept module, EntityProxy.Concept path){
        authoringSTAMP = stampUtility.createSTAMP(status, time, author, module, path);
        starterDataEntities.add(authoringSTAMP);
        return this;
    }

    public StarterData stamp(EntityProxy.Concept status, long time, EntityProxy.Concept author, EntityProxy.Concept module, EntityProxy.Concept path){
        starterDataEntities.add(stampUtility.createSTAMP(status, time, author, module, path));
        return this;
    }

    public ConceptBuilder concept(EntityProxy.Concept concept){
        return new ConceptBuilder(concept);
    }

    public class ConceptBuilder{

        private final List<Entity<? extends EntityVersion>> builderEntities;
        private final int conceptNid;
        private final EntityProxy.Concept conceptProxy;

        protected ConceptBuilder(EntityProxy.Concept concept) {
            this.builderEntities = new ArrayList<>();
            this.conceptNid = concept.nid();
            this.conceptProxy = concept;
            this.builderEntities.add(conceptUtility.createConcept(concept, authoringSTAMP));
        }

        public ConceptBuilder fullyQualifiedName(String text, EntityProxy.Concept dialectAcceptability){
            var fqnSemantic = semanticUtility.createDescriptionSemantic(conceptNid, TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE, text, authoringSTAMP);
            builderEntities.add(fqnSemantic);
            builderEntities.add(semanticUtility.createDialectSemantic(fqnSemantic.nid(), dialectAcceptability, authoringSTAMP));
            return this;
        }

        public ConceptBuilder synonym(String text, EntityProxy.Concept dialectAcceptability){
            var synonymSemantic = semanticUtility.createDescriptionSemantic(conceptNid, TinkarTerm.REGULAR_NAME_DESCRIPTION_TYPE, text, authoringSTAMP);
            builderEntities.add(synonymSemantic);
            builderEntities.add(semanticUtility.createDialectSemantic(synonymSemantic.nid(), dialectAcceptability, authoringSTAMP));
            return this;
        }

        public ConceptBuilder definition(String text, EntityProxy.Concept dialectAcceptability){
            var definitionSemantic = semanticUtility.createDescriptionSemantic(conceptNid, TinkarTerm.DEFINITION_DESCRIPTION_TYPE, text, authoringSTAMP);
            builderEntities.add(definitionSemantic);
            builderEntities.add(semanticUtility.createDialectSemantic(definitionSemantic.nid(), dialectAcceptability, authoringSTAMP));
            return this;
        }

        public ConceptBuilder statedNavigation(List<EntityProxy.Concept> destinations, List<EntityProxy.Concept> origins){
            builderEntities.add(semanticUtility.createNavigationSemantic(conceptNid, TinkarTerm.STATED_NAVIGATION_PATTERN, destinations, origins, authoringSTAMP));
            return this;
        }

        public ConceptBuilder inferredNavigation(List<EntityProxy.Concept> destinations, List<EntityProxy.Concept> origins){
            builderEntities.add(semanticUtility.createNavigationSemantic(conceptNid, TinkarTerm.INFERRED_NAVIGATION_PATTERN, destinations, origins, authoringSTAMP));
            return this;
        }

        public ConceptBuilder identifier(EntityProxy.Concept source, String id){
            builderEntities.add(semanticUtility.createIdentifierSemantic(conceptNid, source, id, authoringSTAMP));
            return this;
        }

        public ConceptBuilder axiomSyntax(String axiomSyntax){
            builderEntities.add(semanticUtility.createAxiomSyntax(conceptNid, axiomSyntax, authoringSTAMP));
            return this;
        }

        public EntityProxy.Concept build(){
            starterDataEntities.addAll(builderEntities);
            return conceptProxy;
        }
    }

    public PatternBuilder pattern(EntityProxy.Pattern pattern){
        return new PatternBuilder(pattern);
    }

    public class PatternBuilder{

        private final int patternNid;
        private final EntityProxy.Pattern patternProxy;
        private EntityProxy.Concept meaning;
        private EntityProxy.Concept purpose;
        private Function<Integer, ImmutableList<FieldDefinitionRecord>> fieldDefinitionFunction;
        private final MutableList<FieldDefinitionRecord> fieldDefinitions;

        protected PatternBuilder(EntityProxy.Pattern pattern) {
            this.patternNid = pattern.nid();
            this.patternProxy = pattern;
            this.fieldDefinitions = Lists.mutable.empty();
        }

        public PatternBuilder meaning(EntityProxy.Concept meaning){
            this.meaning = meaning;
            return this;
        }

        public PatternBuilder purpose(EntityProxy.Concept purpose){
            this.purpose = purpose;
            return this;
        }

        public PatternBuilder fieldDefinition(EntityProxy.Concept meaning, EntityProxy.Concept purpose, EntityProxy.Concept dataType){
            fieldDefinitions.add(FieldDefinitionRecordBuilder.builder()
                    .patternNid(patternNid)
                    .meaningNid(meaning.nid())
                    .purposeNid(purpose.nid())
                    .dataTypeNid(dataType.nid())
                    .indexInPattern(fieldDefinitions.size())
                    .patternVersionStampNid(authoringSTAMP.nid())
                    .build());
            return this;
        }

        public EntityProxy.Pattern build(){
            starterDataEntities.add(patternUtility.createPattern(
                    patternProxy,
                    meaning,
                    purpose,
                    authoringSTAMP,
                    fieldDefinitions.toImmutable()));
            starterDataEntities.add(semanticUtility.createDescriptionSemantic(
                    patternNid,
                    TinkarTerm.FULLY_QUALIFIED_NAME_DESCRIPTION_TYPE,
                    patternProxy.description(),
                    authoringSTAMP));
            //dialect
            return patternProxy;
        }
    }
}
