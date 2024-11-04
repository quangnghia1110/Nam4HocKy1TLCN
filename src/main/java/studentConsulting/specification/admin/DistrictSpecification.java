package studentConsulting.specification.admin;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.DistrictEntity;

public class DistrictSpecification {

    public static Specification<DistrictEntity> hasCode(String code) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("code"), code);
    }

    public static Specification<DistrictEntity> hasName(String name) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("name"), "%" + name + "%");
    }

    public static Specification<DistrictEntity> hasNameEn(String nameEn) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("nameEn"), "%" + nameEn + "%");
    }

    public static Specification<DistrictEntity> hasFullName(String fullName) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("fullName"), "%" + fullName + "%");
    }

    public static Specification<DistrictEntity> hasFullNameEn(String fullNameEn) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("fullNameEn"), "%" + fullNameEn + "%");
    }

    public static Specification<DistrictEntity> hasCodeName(String codeName) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("codeName"), "%" + codeName + "%");
    }

    public static Specification<DistrictEntity> hasProvinceCode(String provinceCode) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("province").get("code"), provinceCode);
    }
}
