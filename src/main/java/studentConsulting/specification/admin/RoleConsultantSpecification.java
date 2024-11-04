package studentConsulting.specification.admin;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.RoleConsultantEntity;

public class RoleConsultantSpecification {

    public static Specification<RoleConsultantEntity> hasName(String name) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.like(root.get("name"), "%" + name + "%");
    }

    public static Specification<RoleConsultantEntity> hasRoleId(Integer roleId) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("role").get("id"), roleId);
    }

    public static Specification<RoleConsultantEntity> hasExactYear(Integer year) {
        return (root, query, criteriaBuilder) -> {
            if (year == null) {
                return criteriaBuilder.conjunction();
            }
            return criteriaBuilder.equal(criteriaBuilder.function("YEAR", Integer.class, root.get("createdAt")), year);
        };
    }
}

