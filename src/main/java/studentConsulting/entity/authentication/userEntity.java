package studentConsulting.entity.authentication;

import javax.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.entity.main.answerEntity;
import studentConsulting.entity.main.departmentEntity;
import studentConsulting.entity.main.questionEntity;

import java.util.List;

@Data
@Builder
@Entity
@Table(name = "user_information") 
@NoArgsConstructor
@AllArgsConstructor
public class userEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "firstname")
    private String firstname;

    @Column(name = "lastname")
    private String lastname;

    @Column(name = "phone", length = 10, nullable = false, unique = true)
    private String phone;

    @Column(name = "occupation", length = 60)
    private String occupation;
    
    @OneToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "account_id")
    private accountEntity accountModel;

    @Column(name = "avatar", columnDefinition = "TEXT")
    private String avatar;
//    
//    @Column(name = "department_id", length = 50)
//    private String departmentId;
//
//    @ManyToOne(fetch = FetchType.LAZY)
//    @JoinColumn(name = "department_id", insertable = false, updatable = false)
//    private departmentEntity department;
//
//    @OneToMany(mappedBy = "user", fetch = FetchType.LAZY)
//    private List<answerEntity> answers;
//
//    @OneToMany(mappedBy = "user", fetch = FetchType.LAZY)
//    private List<questionEntity> questions;


}
