package studentConsulting.specification.actor;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.AccountEntity;
import studentConsulting.model.entity.DepartmentEntity;
import studentConsulting.model.entity.AnswerEntity;
import studentConsulting.model.entity.DeletionLogEntity;
import studentConsulting.model.entity.QuestionEntity;
import studentConsulting.model.entity.UserInformationEntity;

import javax.persistence.criteria.*;
import java.time.LocalDate;

public class QuestionSpecification {

    public static Specification<UserInformationEntity> hasRole(String role) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("account").get("role").get("name"), role);
    }

    public static Specification<UserInformationEntity> hasDepartment(Integer departmentId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("account").get("department").get("id"), departmentId);
    }

    public static Specification<QuestionEntity> hasDepartmentId(Integer departmentId) {
        return (Root<QuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("department").get("id"), departmentId);
        };
    }

    public static Specification<DeletionLogEntity> belongsToDepartment(Integer departmentId) {
        return (Root<DeletionLogEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.equal(root.get("question").get("department").get("id"), departmentId);
    }

    public static Specification<DeletionLogEntity> deletedByEmail(String email) {
        return (Root<DeletionLogEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.equal(root.get("deletedBy"), email);
    }

    public static Specification<DeletionLogEntity> hasQuestionId(Integer questionId) {
        return (Root<DeletionLogEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) ->
                criteriaBuilder.equal(root.get("question").get("id"), questionId);
    }


    public static Specification<UserInformationEntity> hasName(String name) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("firstName"), "%" + name + "%");
    }

    public static Specification<QuestionEntity> hasFieldInDepartment(Integer fieldId, Integer departmentId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.and(
                criteriaBuilder.equal(root.get("field").get("id"), fieldId),
                criteriaBuilder.equal(root.get("field").get("department").get("id"), departmentId)
        );
    }

    public static Specification<QuestionEntity> hasConsultantAnswer(Integer consultantId) {
        return (root, query, cb) -> {
            Join<QuestionEntity, AnswerEntity> answers = root.join("answers");
            return cb.equal(answers.get("user").get("id"), consultantId);
        };
    }

    public static Specification<QuestionEntity> hasDepartments(Integer departmentId) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("department").get("id"), departmentId);

    }

    public static Specification<QuestionEntity> hasNoAnswer() {
        return (root, query, cb) -> cb.isEmpty(root.get("answers"));
    }

    public static Specification<QuestionEntity> hasExactStartDate(LocalDate startDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("createdAt").as(LocalDate.class), startDate);
    }

    public static Specification<QuestionEntity> hasDateBefore(LocalDate endDate) {
        return (root, query, cb) -> cb.lessThanOrEqualTo(root.get("createdAt").as(LocalDate.class), endDate);
    }

    public static Specification<QuestionEntity> hasExactDateRange(LocalDate startDate, LocalDate endDate) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.between(root.get("createdAt").as(LocalDate.class), startDate, endDate);
    }

    public static Specification<QuestionEntity> isDeletedByConsultant() {
        return (Root<QuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            Predicate isDeleted = criteriaBuilder.isTrue(root.get("statusDelete"));
            return criteriaBuilder.and(isDeleted);
        };
    }


    public static Specification<QuestionEntity> hasConsultantAnswer(Integer consultantId, boolean isConsultantSpecific) {
        return (Root<QuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (isConsultantSpecific) {
                // Lấy các câu hỏi đã được tư vấn viên này trả lời
                Join<QuestionEntity, AnswerEntity> answerJoin = root.join("answers");
                Predicate consultantCondition = criteriaBuilder.equal(answerJoin.get("user").get("id"), consultantId);
                Predicate roleCondition = criteriaBuilder.equal(answerJoin.get("user").get("account").get("role").get("name"), SecurityConstants.Role.TUVANVIEN);
                return criteriaBuilder.and(consultantCondition, roleCondition);
            } else {
                // Lấy tất cả các câu hỏi thuộc phòng ban trùng với phòng ban của tư vấn viên
                Join<QuestionEntity, DepartmentEntity> questionDepartmentJoin = root.join("department");

                // Tạo subquery để lấy phòng ban của tư vấn viên
                Subquery<DepartmentEntity> subquery = query.subquery(DepartmentEntity.class);
                Root<UserInformationEntity> consultantRoot = subquery.from(UserInformationEntity.class);

                // Join tới account và department của tư vấn viên
                Join<UserInformationEntity, AccountEntity> accountJoin = consultantRoot.join("account");
                Join<AccountEntity, DepartmentEntity> departmentJoin = accountJoin.join("department");

                // Chọn phòng ban của tư vấn viên
                subquery.select(departmentJoin)
                        .where(criteriaBuilder.equal(consultantRoot.get("id"), consultantId));

                // Điều kiện phòng ban trùng khớp
                Predicate departmentCondition = criteriaBuilder.equal(questionDepartmentJoin, subquery);

                return departmentCondition; // Trả về điều kiện phòng ban trùng
            }
        };
    }


    public static Specification<QuestionEntity> hasUserQuestion(Integer userId) {
        return (Root<QuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            // Điều kiện 1: Lọc theo userId của người đã đặt câu hỏi
            Predicate userCondition = criteriaBuilder.equal(root.get("user").get("id"), userId);

            // Điều kiện 2: Lọc theo vai trò của người dùng là "USER"
            Predicate roleCondition = criteriaBuilder.equal(root.get("user").get("account").get("role").get("name"), SecurityConstants.Role.USER);

            // Trả về kết hợp cả hai điều kiện
            return criteriaBuilder.and(userCondition, roleCondition);
        };
    }


    public static Specification<QuestionEntity> hasConsultantsInDepartment(Integer departmentId) {
        return (root, query, cb) -> {
            if (departmentId != null) {
                return cb.equal(root.get("department").get("id"), departmentId);
            } else {
                return cb.conjunction();
            }
        };
    }


    public static Specification<QuestionEntity> hasTitle(String title) {
        return (Root<QuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (title == null || title.isEmpty()) {
                return null;
            }
            return criteriaBuilder.like(root.get("title"), "%" + title + "%");
        };
    }

    public static Specification<QuestionEntity> hasStatus(QuestionFilterStatus status) {
        return (Root<QuestionEntity> root, CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) -> {
            if (status == null) {
                return null;
            }

            switch (status) {
                case APPROVED:
                    Join<QuestionEntity, AnswerEntity> answerJoinApproved = root.join("answers");
                    return criteriaBuilder.isTrue(answerJoinApproved.get("statusApproval"));
                case PUBLIC:
                    return criteriaBuilder.isTrue(root.get("statusPublic"));
                case PRIVATE:
                    return criteriaBuilder.isFalse(root.get("statusPublic"));
                case DELETED:
                    return criteriaBuilder.isTrue(root.get("statusDelete"));
                case ANSWERED:
                    return criteriaBuilder.isNotNull(root.join("answers"));
                case NOT_ANSWERED:
                    return criteriaBuilder.isNull(root.join("answers", JoinType.LEFT));
                default:
                    return null;
            }
        };
    }

    public static Specification<QuestionEntity> isPublicAndAnswered() {
        return (root, query, criteriaBuilder) -> {
            Predicate isPublic = criteriaBuilder.isTrue(root.get("statusPublic"));
            Predicate isAnswered = criteriaBuilder.isNotNull(root.join("answers", JoinType.LEFT));
            return criteriaBuilder.and(isPublic, isAnswered);
        };
    }

    public static Specification<QuestionEntity> hasExactYear(Integer year) {
        return (root, query, criteriaBuilder) -> {
            if (year == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.equal(criteriaBuilder.function("YEAR", Integer.class, root.get("createdAt")), year);
        };
    }


}

