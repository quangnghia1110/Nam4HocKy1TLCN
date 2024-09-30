package studentConsulting.specification.question_answer;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.entity.authentication.RoleEntity;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.question_answer.CommonQuestionEntity;
import studentConsulting.model.entity.user.UserInformationEntity;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Root;
import java.time.LocalDate;

public class CommonQuestionSpecification {

    public static Specification<CommonQuestionEntity> isCreatedByAdvisor(Integer departmentId) {
        return (root, query, cb) -> {
            Join<CommonQuestionEntity, UserInformationEntity> userJoin = root.join("createdBy");
            Join<UserInformationEntity, AccountEntity> accountJoin = userJoin.join("account");
            Join<AccountEntity, RoleEntity> roleJoin = accountJoin.join("role");
            Join<AccountEntity, DepartmentEntity> departmentJoin = accountJoin.join("department");
            return cb.and(
                    cb.equal(roleJoin.get("name"), "ROLE_TRUONGBANTUVAN"),
                    cb.equal(departmentJoin.get("id"), departmentId)
            );
        };
    }

    public static Specification<CommonQuestionEntity> hasDepartment(Integer departmentId) {
        return (Root<CommonQuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (departmentId == null) {
                return null;
            }
            return criteriaBuilder.equal(root.get("department").get("id"), departmentId);
        };
    }

    public static Specification<CommonQuestionEntity> hasTitle(String title) {
        return (Root<CommonQuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (title == null || title.isEmpty()) {
                return null;
            }
            return criteriaBuilder.like(root.get("title"), "%" + title + "%");
        };
    }

    public static Specification<CommonQuestionEntity> hasExactStartDate(LocalDate startDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("createdAt").as(LocalDate.class), startDate);
    }

    public static Specification<CommonQuestionEntity> hasDateBefore(LocalDate endDate) {
        return (root, query, cb) -> cb.lessThanOrEqualTo(root.get("createdAt").as(LocalDate.class), endDate);
    }

    public static Specification<CommonQuestionEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.between(root.get("createdAt").as(LocalDate.class), startDate, endDate);
    }
}
