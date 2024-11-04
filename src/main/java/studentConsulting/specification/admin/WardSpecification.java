package studentConsulting.specification.admin;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.WardEntity;

public class WardSpecification {

    public static Specification<WardEntity> hasCode(String code) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("code"), code);
    }

    public static Specification<WardEntity> hasName(String name) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("name"), "%" + name + "%");
    }

    public static Specification<WardEntity> hasNameEn(String nameEn) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("nameEn"), "%" + nameEn + "%");
    }

    public static Specification<WardEntity> hasFullName(String fullName) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("fullName"), "%" + fullName + "%");
    }

    public static Specification<WardEntity> hasFullNameEn(String fullNameEn) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("fullNameEn"), "%" + fullNameEn + "%");
    }

    public static Specification<WardEntity> hasCodeName(String codeName) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("codeName"), "%" + codeName + "%");
    }

    public static Specification<WardEntity> hasDistrictCode(String districtCode) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("district").get("code"), districtCode);
    }
}
