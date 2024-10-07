package studentConsulting.specification.authentication;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.user.RoleAskEntity;

import javax.persistence.criteria.JoinType;

public class RoleAskSpecification {

    public static Specification<RoleAskEntity> hasName(String name) {
        return (root, query, criteriaBuilder) ->
                criteriaBuilder.like(root.get("name"), "%" + name + "%");
    }

    public static Specification<RoleAskEntity> hasRoleId(Integer roleId) {
        return (root, query, criteriaBuilder) -> {
            return criteriaBuilder.equal(
                    root.join("role", JoinType.INNER).get("id"), roleId
            );
        };
    }
}
