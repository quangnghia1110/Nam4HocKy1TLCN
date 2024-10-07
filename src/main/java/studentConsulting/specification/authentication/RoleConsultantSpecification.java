package studentConsulting.specification.authentication;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.user.RoleConsultantEntity;

public class RoleConsultantSpecification {

    public static Specification<RoleConsultantEntity> hasName(String name) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.like(root.get("name"), "%" + name + "%");
    }

    public static Specification<RoleConsultantEntity> hasRoleId(Integer roleId) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.equal(root.get("role").get("id"), roleId);
    }
}

