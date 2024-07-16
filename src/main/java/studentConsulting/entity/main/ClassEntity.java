package studentConsulting.entity.main;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@Entity
@Table(name = "classes")
@NoArgsConstructor
@AllArgsConstructor
public class ClassEntity {

    @Id
    @Column(name = "id", length = 50, nullable = false)
    private String id;

    @Column(name = "name", length = 255, nullable = false)
    private String name;

    @Column(name = "description", columnDefinition = "TEXT")
    private String description;

    @Column(name = "status", nullable = false)
    private boolean status = true;

    @Column(name = "department_id", length = 50, nullable = false)
    private String departmentId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "department_id", insertable = false, updatable = false)
    private DepartmentEntity department;

    @OneToMany(mappedBy = "classEntity", fetch = FetchType.LAZY)
    private List<ClassUserEntity> classUsers;
}
