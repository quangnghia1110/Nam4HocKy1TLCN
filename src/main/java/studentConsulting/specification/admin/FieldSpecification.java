package studentConsulting.specification.admin;


import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.FieldEntity;

public class FieldSpecification {

    public static Specification<FieldEntity> hasName(String name) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.like(root.get("name"), "%" + name + "%");
    }

    public static Specification<FieldEntity> hasDepartmentId(Integer departmentId) {
        return (root, query, criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("department").get("id"), departmentId);
        };
    }

    public static Specification<FieldEntity> hasExactYear(Integer year) {
        return (root, query, criteriaBuilder) -> {
            if (year == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.equal(criteriaBuilder.function("YEAR", Integer.class, root.get("createdAt")), year);
        };
    }

    public static Specification<FieldEntity> hasDepartment(Integer departmentId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("department").get("id"), departmentId);
    }
}

