package studentConsulting.specification.communication;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.communication.MessageEntity;

public class MessageSpecification {
    public static Specification<MessageEntity> hasConversationId(Integer conversationId) {
        return (root, query, builder) -> builder.equal(root.get("conversationId"), conversationId);
    }

    public static Specification<MessageEntity> hasExactYear(Integer year) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(
                criteriaBuilder.function("YEAR", Integer.class, root.get("date")), year);
    }

    public static Specification<MessageEntity> isSentByConsultant(Integer consultantId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("sender").get("id"), consultantId);
    }

    public static Specification<MessageEntity> isSentByDepartment(Integer departmentId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(
                root.join("sender").join("account").join("department").get("id"), departmentId);
    }
}

