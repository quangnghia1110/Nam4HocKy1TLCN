package studentConsulting.specification;

import java.time.LocalDate;

import org.springframework.data.jpa.domain.Specification;

import studentConsulting.model.entity.communication.ConversationEntity;

public class ConversationSpecification {

    public static Specification<ConversationEntity> hasUser(Integer userId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("user").get("id"), userId);
    }

    public static Specification<ConversationEntity> hasConsultant(Integer consultantId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("consultant").get("id"), consultantId);
    }

    public static Specification<ConversationEntity> hasRoleUser() {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("user").get("account").get("role").get("name"), "ROLE_USER");
    }

    public static Specification<ConversationEntity> hasRoleConsultant() {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("consultant").get("account").get("role").get("name"), "ROLE_TUVANVIEN");
    }

    public static Specification<ConversationEntity> hasName(String name) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("name")), "%" + name.toLowerCase() + "%");
    }

    public static Specification<ConversationEntity> hasExactStartDate(LocalDate startDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("createdAt").as(LocalDate.class), startDate);
    }

    public static Specification<ConversationEntity> hasDateBefore(LocalDate endDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.lessThanOrEqualTo(root.get("createdAt").as(LocalDate.class), endDate);
    }

    public static Specification<ConversationEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.between(root.get("createdAt").as(LocalDate.class), startDate, endDate);
    }
}



