package studentConsulting.entity.main;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.entity.authentication.userEntity;

import java.util.List;

@Data
@Builder
@Entity
@Table(name = "departments")
@NoArgsConstructor
@AllArgsConstructor
public class departmentEntity {

    @Id
    @Column(name = "id", length = 50, nullable = false)
    private String id;

    @Column(name = "name", length = 255, nullable = false)
    private String name;

    @Column(name = "description", columnDefinition = "TEXT")
    private String description;

    @Column(name = "status", nullable = false)
    private boolean status = true;

//    @OneToMany(mappedBy = "department", fetch = FetchType.LAZY)
//    private List<userEntity> users;

    @OneToMany(mappedBy = "department", fetch = FetchType.LAZY)
    private List<questionEntity> questions;

    @OneToMany(mappedBy = "department", fetch = FetchType.LAZY)
    private List<classEntity> classes;
}
