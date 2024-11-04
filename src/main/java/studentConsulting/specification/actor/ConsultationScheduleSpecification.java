package studentConsulting.specification.actor;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.RoleEntity;
import studentConsulting.model.entity.ConsultationScheduleEntity;
import studentConsulting.model.entity.UserInformationEntity;

import javax.persistence.criteria.Join;
import java.time.LocalDate;

public class ConsultationScheduleSpecification {

    public static Specification<ConsultationScheduleEntity> hasDepartment(Integer departmentId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("department").get("id"), departmentId);
    }

    public static Specification<ConsultationScheduleEntity> hasUser(UserInformationEntity user) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("user"), user);
    }

    public static Specification<ConsultationScheduleEntity> hasUser(Integer userId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("user").get("id"), userId);
    }

    public static Specification<ConsultationScheduleEntity> hasConsultant(Integer consultantId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("consultant").get("id"), consultantId);
    }

    public static Specification<ConsultationScheduleEntity> hasConsultant(UserInformationEntity consultant) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("consultant"), consultant);
    }

    public static Specification<ConsultationScheduleEntity> hasTitle(String title) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("title"), "%" + title + "%");
    }

    public static Specification<ConsultationScheduleEntity> hasStatusPublic(Boolean statusPublic) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("statusPublic"), statusPublic);
    }

    public static Specification<ConsultationScheduleEntity> hasStatusConfirmed(Boolean statusConfirmed) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("statusConfirmed"), statusConfirmed);
    }

    public static Specification<ConsultationScheduleEntity> hasMode(Boolean mode) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("mode"), mode);
    }

    public static Specification<ConsultationScheduleEntity> hasExactStartDate(LocalDate startDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("createdAt").as(LocalDate.class), startDate);
    }

    public static Specification<ConsultationScheduleEntity> hasDateBefore(LocalDate endDate) {
        return (root, query, cb) -> cb.lessThanOrEqualTo(root.get("createdAt").as(LocalDate.class), endDate);
    }

    public static Specification<ConsultationScheduleEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.between(root.get("createdAt").as(LocalDate.class), startDate, endDate);
    }

    public static Specification<ConsultationScheduleEntity> isNotCreatedByRole(String role) {
        return (root, query, criteriaBuilder) -> {
            Join<ConsultationScheduleEntity, UserInformationEntity> consultant = root.join("consultant");
            Join<UserInformationEntity, RoleEntity> roleEntity = consultant.join("account").join("roleConsultant");
            return criteriaBuilder.notEqual(roleEntity.get("name"), role);
        };
    }

    public static Specification<ConsultationScheduleEntity> isCreatedByAdvisor() {
        return (root, query, criteriaBuilder) -> criteriaBuilder.and(
                criteriaBuilder.isNotNull(root.get("department").get("id")),
                criteriaBuilder.isNull(root.get("consultant").get("id")),
                criteriaBuilder.isNull(root.get("user").get("id"))
        );
    }

    public static Specification<ConsultationScheduleEntity> hasExactYear(Integer year) {
        return (root, query, criteriaBuilder) -> {
            if (year == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.equal(criteriaBuilder.function("YEAR", Integer.class, root.get("createdAt")), year);
        };
    }


}
