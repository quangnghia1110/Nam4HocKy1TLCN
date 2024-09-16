package studentConsulting.specification;

import org.springframework.data.jpa.domain.Specification;

import studentConsulting.model.entity.communication.MessageEntity;

public class MessageSpecification {
    public static Specification<MessageEntity> hasConversationId(Integer conversationId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("conversation").get("id"), conversationId);
    }
}

