package studentConsulting.specification.department_field;


import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.department_field.FieldEntity;

public class FieldSpecification {

    public static Specification<FieldEntity> hasName(String name) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.like(root.get("name"), "%" + name + "%");
    }

    public static Specification<FieldEntity> hasDepartmentId(String departmentId) {
        return (root, query, criteriaBuilder) -> {
            return criteriaBuilder.equal(root.get("department").get("id"), departmentId);
        };
    }
}

