package studentConsulting.specification.admin;

import org.springframework.data.jpa.domain.Specification;
import studentConsulting.model.entity.ProvinceEntity;

public class ProvinceSpecification {

    public static Specification<ProvinceEntity> hasName(String name) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("name"), "%" + name + "%");
    }

    public static Specification<ProvinceEntity> hasNameEn(String nameEn) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("nameEn"), "%" + nameEn + "%");
    }

    public static Specification<ProvinceEntity> hasFullName(String fullName) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("fullName"), "%" + fullName + "%");
    }

    public static Specification<ProvinceEntity> hasFullNameEn(String fullNameEn) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("fullNameEn"), "%" + fullNameEn + "%");
    }

    public static Specification<ProvinceEntity> hasCodeName(String codeName) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(root.get("codeName"), "%" + codeName + "%");
    }

    public static Specification<ProvinceEntity> hasCode(String code) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("code"), code);
    }
}
