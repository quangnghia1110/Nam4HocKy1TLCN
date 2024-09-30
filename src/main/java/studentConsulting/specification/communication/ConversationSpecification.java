package studentConsulting.specification.communication;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.ConversationUserEntity;

import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;
import java.time.LocalDate;

public class ConversationSpecification {

    public static Specification<ConversationEntity> hasDepartment(Integer departmentId) {
        return (root, query, criteriaBuilder) -> {
            if (departmentId == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.equal(root.get("department").get("id"), departmentId);
        };
    }

    public static Specification<ConversationEntity> isOwner(Integer userId) {
        return (root, query, builder) -> builder.or(
                builder.equal(root.get("user").get("id"), userId)
        );
    }

    public static Specification<ConversationEntity> isMember(Integer userId) {
        return (root, query, builder) -> {
            Subquery<ConversationUserEntity> subquery = query.subquery(ConversationUserEntity.class);
            Root<ConversationUserEntity> conversationUser = subquery.from(ConversationUserEntity.class);

            subquery.select(conversationUser).where(
                    builder.equal(conversationUser.get("conversation").get("id"), root.get("id")),
                    builder.equal(conversationUser.get("user").get("id"), userId)
            );

            return builder.exists(subquery);
        };
    }


    public static Specification<ConversationEntity> hasUser(Integer userId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("user").get("id"), userId);
    }

    public static Specification<ConversationEntity> hasConsultant(Integer consultantId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("consultant").get("id"), consultantId);
    }

    public static Specification<ConversationEntity> hasRoleUser() {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("user").get("account").get("role").get("name"), SecurityConstants.Role.USER);
    }

    public static Specification<ConversationEntity> hasRoleConsultant() {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("consultant").get("account").get("role").get("name"), SecurityConstants.Role.TUVANVIEN);
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

    public static Specification<ConversationEntity> hasExactYear(Integer year) {
        return (root, query, criteriaBuilder) -> {
            if (year == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.equal(criteriaBuilder.function("YEAR", Integer.class, root.get("createdAt")), year);
        };
    }

}



